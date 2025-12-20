use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::values::{BasicValueEnum, FunctionValue, PointerValue};
use std::collections::HashMap;

use crate::parser::{AstNode, Expression, Program};

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

    // Runtime functions like println and print
    runtime_functions: HashMap<&'ctx str, FunctionValue<'ctx>>,
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
            runtime_functions: HashMap::new(),
        }
    }

    pub fn declare_runtime_functions(&mut self) {
        let i64_type = self.context.i64_type();
        let f64_type = self.context.f64_type();
        let bool_type = self.context.bool_type();

        let signatures = [
            ("print_i64", i64_type.fn_type(&[i64_type.into()], false)),
            ("print_f64", f64_type.fn_type(&[f64_type.into()], false)),
            ("print_bool", bool_type.fn_type(&[bool_type.into()], false)),
        ];

        for (name, fn_type) in signatures {
            let func = self
                .module
                .add_function(name, fn_type, Some(Linkage::External));
            self.runtime_functions.insert(name, func);
        }
    }

    fn compile_expression_to_value(&mut self, expr: Expression) -> BasicValueEnum<'ctx> {
        match expr {
            Expression::Literal(val) => self.context.i64_type().const_int(val as u64, false).into(),

            Expression::FunctionCall {
                function_name,
                params,
            } => {
                if function_name == "println" {
                    let arg = self.compile_expression_to_value(params[0].clone());

                    let print_fn = *self
                        .runtime_functions
                        .get("print_i64")
                        .expect("print_i64 not declared");

                    self.builder
                        .build_call(print_fn, &[arg.into()], "")
                        .unwrap();

                    // println returns void → return dummy value
                    self.context.i64_type().const_int(0, false).into()
                } else {
                    panic!("Unknown function {}", function_name)
                }
            }
            _ => todo!(),
        }
    }

    fn compile_function_decl(&mut self, name: &str, body: AstNode) {
        let i64_type = self.context.i64_type();
        let fn_type = i64_type.fn_type(&[], false);

        let function = self.module.add_function(name, fn_type, None);
        self.functions.insert(name.to_string(), function);

        let entry = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry);

        self.current_function = Some(function);

        self.compile_node(body);

        self.builder
            .build_return(Some(&i64_type.const_int(0, false)))
            .unwrap();
    }

    fn compile_node(&mut self, node: AstNode) {
        match node {
            AstNode::Expr(expr) => {
                self.compile_expression_to_value(expr);
            }
        }
    }

    pub fn compile_program(&mut self, program: Program) {
        self.declare_runtime_functions();

        for node in program.body {
            if let AstNode::Expr(Expression::FunctionDecl { name, body, .. }) = node {
                self.compile_function_decl(&name, *body);
            }
        }
    }

    pub fn save_ir(&mut self) {
        self.module
            .print_to_file("output.ll")
            .expect("Failed to save IR");
    }
}
