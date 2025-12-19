use inkwell::IntPredicate;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};
use std::collections::HashMap;

// Import your AST types
use crate::lexer::Token;
use crate::parser::{AstNode, Expression, Params, Program, Type};

pub struct CodeGen<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,

    // Symbol table for variables (maps variable name to pointer)
    variables: HashMap<String, PointerValue<'ctx>>,

    // Function table (maps function name to FunctionValue)
    functions: HashMap<String, FunctionValue<'ctx>>,

    // Current function being compiled
    current_function: Option<FunctionValue<'ctx>>,
}

impl<'ctx> CodeGen<'ctx> {
    pub fn new(context: &'ctx Context, module_name: &str) -> Self {
        let module = context.create_module(module_name);
        let builder = context.create_builder();

        Self {
            context,
            module,
            builder,
            variables: HashMap::new(),
            functions: HashMap::new(),
            current_function: None,
        }
    }

    pub fn compile_program(&mut self, program: Program) -> Result<(), String> {
        for node in program.body {
            self.compile_top_level(node)?;
        }
        Ok(())
    }

    fn compile_top_level(&mut self, node: AstNode) -> Result<(), String> {
        match node {
            AstNode::Expr(Expression::FunctionDecl {
                name, params, body, ..
            }) => {
                self.compile_function_decl(&name, &params, &body)?;
                Ok(())
            }
            _ => Err("Only function declarations allowed at top level".to_string()),
        }
    }

    fn compile_function_decl(
        &mut self,
        name: &str,
        params: &[Params],
        body: &AstNode,
    ) -> Result<FunctionValue<'ctx>, String> {
        // Create function signature (for now, all params and return are i64)
        let i64_type = self.context.i64_type();
        let param_types: Vec<BasicMetadataTypeEnum> =
            params.iter().map(|_| i64_type.into()).collect();

        let fn_type = i64_type.fn_type(&param_types, false);
        let function = self.module.add_function(name, fn_type, None);

        // Store function in symbol table
        self.functions.insert(name.to_string(), function);

        // Create entry basic block
        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        // Save current function
        self.current_function = Some(function);

        // Create new scope for function variables
        let old_variables = self.variables.clone();
        self.variables.clear();

        // Allocate and store parameters
        for (i, param) in params.iter().enumerate() {
            let param_value = function.get_nth_param(i as u32).unwrap().into_int_value();
            let alloca = self
                .builder
                .build_alloca(i64_type, &param.name)
                .map_err(|e| format!("Failed to allocate parameter: {:?}", e))?;
            self.builder
                .build_store(alloca, param_value)
                .map_err(|e| format!("Failed to store parameter: {:?}", e))?;
            self.variables.insert(param.name.clone(), alloca);
        }

        // Compile function body
        let return_value = self.compile_expression(body)?;

        // If the last instruction isn't a return, add one
        if self
            .builder
            .get_insert_block()
            .unwrap()
            .get_terminator()
            .is_none()
        {
            self.builder
                .build_return(Some(&return_value))
                .map_err(|e| format!("Failed to build return: {:?}", e))?;
        }

        // Restore previous scope
        self.variables = old_variables;
        self.current_function = None;

        Ok(function)
    }

    fn compile_expression(&mut self, node: &AstNode) -> Result<BasicValueEnum<'ctx>, String> {
        match node {
            AstNode::Expr(expr) => self.compile_expr(expr),
        }
    }

    fn compile_expr(&mut self, expr: &Expression) -> Result<BasicValueEnum<'ctx>, String> {
        match expr {
            Expression::Literal(val) => {
                let i64_type = self.context.i64_type();
                Ok(i64_type.const_int(*val as u64, true).into())
            }

            Expression::Variable(name) => {
                let ptr = self
                    .variables
                    .get(name)
                    .ok_or(format!("Undefined variable: {}", name))?;
                let value = self
                    .builder
                    .build_load(self.context.i64_type(), *ptr, name)
                    .map_err(|e| format!("Failed to load variable: {:?}", e))?;
                Ok(value)
            }

            Expression::Binary {
                left,
                operator,
                right,
                ..
            } => {
                let lhs = self.compile_expression(left)?.into_int_value();
                let rhs = self.compile_expression(right)?.into_int_value();

                let result = match operator {
                    Token::Plus => self
                        .builder
                        .build_int_add(lhs, rhs, "addtmp")
                        .map_err(|e| format!("Failed to build add: {:?}", e))?,
                    Token::Minus => self
                        .builder
                        .build_int_sub(lhs, rhs, "subtmp")
                        .map_err(|e| format!("Failed to build sub: {:?}", e))?,
                    Token::Star => self
                        .builder
                        .build_int_mul(lhs, rhs, "multmp")
                        .map_err(|e| format!("Failed to build mul: {:?}", e))?,
                    Token::Slash => self
                        .builder
                        .build_int_signed_div(lhs, rhs, "divtmp")
                        .map_err(|e| format!("Failed to build div: {:?}", e))?,
                    Token::EqualEqual => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::EQ, lhs, rhs, "eqtmp")
                            .map_err(|e| format!("Failed to build comparison: {:?}", e))?;
                        self.builder
                            .build_int_z_extend(cmp, self.context.i64_type(), "booltmp")
                            .map_err(|e| format!("Failed to extend bool: {:?}", e))?
                    }
                    Token::NotEqual => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::NE, lhs, rhs, "neqtmp")
                            .map_err(|e| format!("Failed to build comparison: {:?}", e))?;
                        self.builder
                            .build_int_z_extend(cmp, self.context.i64_type(), "booltmp")
                            .map_err(|e| format!("Failed to extend bool: {:?}", e))?
                    }
                    Token::LessThan => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::SLT, lhs, rhs, "lttmp")
                            .map_err(|e| format!("Failed to build comparison: {:?}", e))?;
                        self.builder
                            .build_int_z_extend(cmp, self.context.i64_type(), "booltmp")
                            .map_err(|e| format!("Failed to extend bool: {:?}", e))?
                    }
                    Token::LessThanOrEqual => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::SLE, lhs, rhs, "letmp")
                            .map_err(|e| format!("Failed to build comparison: {:?}", e))?;
                        self.builder
                            .build_int_z_extend(cmp, self.context.i64_type(), "booltmp")
                            .map_err(|e| format!("Failed to extend bool: {:?}", e))?
                    }
                    Token::GreaterThan => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::SGT, lhs, rhs, "gttmp")
                            .map_err(|e| format!("Failed to build comparison: {:?}", e))?;
                        self.builder
                            .build_int_z_extend(cmp, self.context.i64_type(), "booltmp")
                            .map_err(|e| format!("Failed to extend bool: {:?}", e))?
                    }
                    Token::GreaterThanOrEqual => {
                        let cmp = self
                            .builder
                            .build_int_compare(IntPredicate::SGE, lhs, rhs, "getmp")
                            .map_err(|e| format!("Failed to build comparison: {:?}", e))?;
                        self.builder
                            .build_int_z_extend(cmp, self.context.i64_type(), "booltmp")
                            .map_err(|e| format!("Failed to extend bool: {:?}", e))?
                    }
                    Token::And => {
                        // Logical AND: convert to boolean (!=0), then AND
                        let lhs_bool = self
                            .builder
                            .build_int_compare(
                                IntPredicate::NE,
                                lhs,
                                self.context.i64_type().const_zero(),
                                "lhsbool",
                            )
                            .map_err(|e| format!("Failed to build comparison: {:?}", e))?;
                        let rhs_bool = self
                            .builder
                            .build_int_compare(
                                IntPredicate::NE,
                                rhs,
                                self.context.i64_type().const_zero(),
                                "rhsbool",
                            )
                            .map_err(|e| format!("Failed to build comparison: {:?}", e))?;
                        let and_result = self
                            .builder
                            .build_and(lhs_bool, rhs_bool, "andtmp")
                            .map_err(|e| format!("Failed to build and: {:?}", e))?;
                        self.builder
                            .build_int_z_extend(and_result, self.context.i64_type(), "booltmp")
                            .map_err(|e| format!("Failed to extend bool: {:?}", e))?
                    }
                    Token::Or => {
                        // Logical OR: convert to boolean (!=0), then OR
                        let lhs_bool = self
                            .builder
                            .build_int_compare(
                                IntPredicate::NE,
                                lhs,
                                self.context.i64_type().const_zero(),
                                "lhsbool",
                            )
                            .map_err(|e| format!("Failed to build comparison: {:?}", e))?;
                        let rhs_bool = self
                            .builder
                            .build_int_compare(
                                IntPredicate::NE,
                                rhs,
                                self.context.i64_type().const_zero(),
                                "rhsbool",
                            )
                            .map_err(|e| format!("Failed to build comparison: {:?}", e))?;
                        let or_result = self
                            .builder
                            .build_or(lhs_bool, rhs_bool, "ortmp")
                            .map_err(|e| format!("Failed to build or: {:?}", e))?;
                        self.builder
                            .build_int_z_extend(or_result, self.context.i64_type(), "booltmp")
                            .map_err(|e| format!("Failed to extend bool: {:?}", e))?
                    }
                    _ => return Err(format!("Unsupported binary operator: {:?}", operator)),
                };

                Ok(result.into())
            }

            Expression::Let {
                var_name, value, ..
            } => {
                let val = self.compile_expression(value)?;
                let i64_type = self.context.i64_type();

                let alloca = self
                    .builder
                    .build_alloca(i64_type, var_name)
                    .map_err(|e| format!("Failed to allocate variable: {:?}", e))?;
                self.builder
                    .build_store(alloca, val)
                    .map_err(|e| format!("Failed to store variable: {:?}", e))?;

                self.variables.insert(var_name.clone(), alloca);

                // Let expressions return unit (0)
                Ok(i64_type.const_zero().into())
            }

            Expression::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let cond_value = self.compile_expression(condition)?.into_int_value();

                // Convert condition to boolean (non-zero = true)
                let cond_bool = self
                    .builder
                    .build_int_compare(
                        IntPredicate::NE,
                        cond_value,
                        self.context.i64_type().const_zero(),
                        "ifcond",
                    )
                    .map_err(|e| format!("Failed to build condition: {:?}", e))?;

                let function = self.current_function.unwrap();

                let then_bb = self.context.append_basic_block(function, "then");
                let else_bb = self.context.append_basic_block(function, "else");
                let merge_bb = self.context.append_basic_block(function, "ifcont");

                self.builder
                    .build_conditional_branch(cond_bool, then_bb, else_bb)
                    .map_err(|e| format!("Failed to build conditional branch: {:?}", e))?;

                // Compile then branch
                self.builder.position_at_end(then_bb);
                let then_val = self.compile_expression(then_branch)?;
                self.builder
                    .build_unconditional_branch(merge_bb)
                    .map_err(|e| format!("Failed to build branch: {:?}", e))?;
                let then_bb = self.builder.get_insert_block().unwrap();

                // Compile else branch
                self.builder.position_at_end(else_bb);
                let else_val = if let Some(else_expr) = else_branch {
                    self.compile_expression(else_expr)?
                } else {
                    self.context.i64_type().const_zero().into()
                };
                self.builder
                    .build_unconditional_branch(merge_bb)
                    .map_err(|e| format!("Failed to build branch: {:?}", e))?;
                let else_bb = self.builder.get_insert_block().unwrap();

                // Merge block with phi
                self.builder.position_at_end(merge_bb);
                let phi = self
                    .builder
                    .build_phi(self.context.i64_type(), "iftmp")
                    .map_err(|e| format!("Failed to build phi: {:?}", e))?;
                phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

                Ok(phi.as_basic_value())
            }

            Expression::FunctionCall {
                function_name,
                params,
            } => {
                let function = *self
                    .functions
                    .get(function_name)
                    .ok_or(format!("Undefined function: {}", function_name))?;

                let args = {
                    let mut args = Vec::new();
                    for param in params {
                        let arg = self.compile_expr(param)?;
                        args.push(arg.into());
                    }
                    args
                };

                let call = self
                    .builder
                    .build_call(function, &args, "calltmp")
                    .map_err(|e| format!("Failed to build call: {:?}", e))?;

                Ok(call.try_as_basic_value().unwrap_basic())
            }

            Expression::ClosureFunction { params, body, .. } => {
                // For closures, we need to create an anonymous function
                // This is a simplified version - real closures need capture handling

                let closure_name = format!("closure_{}", self.functions.len());

                // Save current state
                let old_function = self.current_function;
                let old_vars = self.variables.clone();

                // Create closure function
                let closure_fn = self.compile_function_decl(&closure_name, params, body)?;

                // Restore state
                self.current_function = old_function;
                self.variables = old_vars;

                // For now, return the function pointer as an i64
                // In a real implementation, you'd need proper function pointer handling
                let fn_ptr = closure_fn.as_global_value().as_pointer_value();
                let ptr_as_int = self
                    .builder
                    .build_ptr_to_int(fn_ptr, self.context.i64_type(), "fn_ptr")
                    .map_err(|e| format!("Failed to convert pointer: {:?}", e))?;

                // Store closure in variables for later use
                let alloca = self
                    .builder
                    .build_alloca(self.context.i64_type(), &closure_name)
                    .map_err(|e| format!("Failed to allocate closure: {:?}", e))?;
                self.builder
                    .build_store(alloca, ptr_as_int)
                    .map_err(|e| format!("Failed to store closure: {:?}", e))?;

                // Find the variable name this closure is being assigned to
                // This is a hack - in reality you'd track this better
                Ok(ptr_as_int.into())
            }

            Expression::Block { body, .. } => {
                let mut last_value = self.context.i64_type().const_zero().into();
                for stmt in body {
                    last_value = self.compile_expression(stmt)?;
                }
                Ok(last_value)
            }

            Expression::Return { expr } => {
                let ret_val = self.compile_expression(expr)?;
                self.builder
                    .build_return(Some(&ret_val))
                    .map_err(|e| format!("Failed to build return: {:?}", e))?;
                Ok(ret_val)
            }

            _ => Err(format!("Unsupported expression: {:?}", expr)),
        }
    }

    pub fn print_ir(&self) {
        self.module.print_to_stderr();
    }

    pub fn get_module(&self) -> &Module<'ctx> {
        &self.module
    }
}

// Example usage
pub fn compile_and_run(program: Program) -> Result<(), String> {
    let context = Context::create();
    let mut codegen = CodeGen::new(&context, "my_module");

    codegen.compile_program(program)?;
    codegen
        .module
        .print_to_file("output.ll")
        .expect("Compilation failed");
    Ok(())
}
