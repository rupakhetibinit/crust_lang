use crust_frontend::ast::LiteralValue;

use crate::codegen::OpCode;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Chunk {
    pub code: Vec<OpCode>,
    pub constants: Vec<LiteralValue>,
    pub locals: Vec<String>,
    pub functions: HashMap<String, usize>,
    unresolved_calls: Vec<(usize, String)>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: vec![],
            constants: vec![],
            locals: vec![],
            functions: HashMap::new(),
            unresolved_calls: Vec::new(),
        }
    }

    pub fn add_constant(&mut self, val: LiteralValue) -> usize {
        self.constants.push(val);
        self.constants.len() - 1
    }

    pub fn emit(&mut self, op: OpCode) {
        self.code.push(op);
    }

    pub fn emit_call(&mut self, name: String, argument_count: usize) {
        let index = self.code.len();
        self.emit(OpCode::Call(usize::MAX, argument_count));
        self.unresolved_calls.push((index, name));
    }

    pub fn resolve_calls(&mut self) {
        for (index, name) in self.unresolved_calls.drain(..) {
            let target = *self.functions.get(&name).expect("unknown function");
            match &mut self.code[index] {
                OpCode::Call(address, _) => *address = target,
                _ => unreachable!(),
            }
        }
    }

    pub fn patch_jump(&mut self, index: usize, target: usize) {
        match &mut self.code[index] {
            OpCode::JumpIfFalse(address) | OpCode::Jump(address) => *address = target,
            _ => panic!("cannot patch non-jump opcode"),
        }
    }

    pub fn declare_variable(&mut self, name: String) -> usize {
        self.locals.push(name);
        self.locals.len() - 1
    }

    pub fn resolve_variable(&self, name: &str) -> usize {
        self.locals
            .iter()
            .rposition(|s| s == name)
            .expect("undeclared variable")
    }
    pub fn disassemble(&self) {
        println!("== Chunk ==");
        for (i, op) in self.code.iter().enumerate() {
            print!("{:04} ", i);
            match op {
                OpCode::LoadConst(idx) => {
                    println!(
                        "LOAD_CONST   {} ({:?})",
                        idx,
                        self.constants.get(*idx).unwrap()
                    );
                }
                OpCode::LoadLocal(idx) => {
                    println!("LOAD_LOCAL   {}", idx);
                }
                OpCode::StoreLocal(idx) => {
                    println!("STORE_LOCAL  {}", idx);
                }
                OpCode::AddI64 => {
                    println!("ADD_I64");
                }
                OpCode::AddF64 => {
                    println!("ADD_F64");
                }
                OpCode::SubI64 => {
                    println!("SUB_I64");
                }
                OpCode::EqualI64 => {
                    println!("EQUAL_I64");
                }
                OpCode::LessI64 => {
                    println!("LESS_I64");
                }
                OpCode::LessEqualI64 => {
                    println!("LESS_EQUAL_I64");
                }
                OpCode::GreaterI64 => {
                    println!("GREATER_I64");
                }
                OpCode::GreaterEqualI64 => {
                    println!("GREATER_EQUAL_I64");
                }
                OpCode::MulI64 => {
                    println!("MUL_I64");
                }
                OpCode::MulF64 => {
                    println!("MUL_F64");
                }
                OpCode::NegI64 => {
                    println!("NEG_I64");
                }
                OpCode::DivF64 => {
                    println!("DIV_F64");
                }
                OpCode::DivI64 => {
                    println!("DIV_I64");
                }
                OpCode::Call(target, argument_count) => {
                    println!("CALL         {} ({})", target, argument_count);
                }
                OpCode::Return => {
                    println!("RETURN");
                }
                OpCode::Halt => {
                    println!("HALT");
                }
                OpCode::Print(argument_count) => {
                    println!("PRINT        {}", argument_count);
                }
                OpCode::IncrementLocal(idx) => {
                    println!("INCREMENT_LOCAL {}", idx);
                }
                OpCode::DecrementLocal(idx) => {
                    println!("DECREMENT_LOCAL {}", idx);
                }
                OpCode::JumpIfFalse(target) => {
                    println!("JUMP_IF_FALSE {}", target);
                }
                OpCode::Jump(target) => {
                    println!("JUMP         {}", target);
                }
            }
        }

        println!("\nConstants:");
        for (i, c) in self.constants.iter().enumerate() {
            println!("  [{}] {:?}", i, c);
        }
    }
}
