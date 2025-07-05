use ast::LiteralValue;

use crate::codegen::OpCode;

#[derive(Debug)]
pub struct Chunk {
    pub code: Vec<OpCode>,
    pub constants: Vec<LiteralValue>,
    pub locals: Vec<String>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: vec![],
            constants: vec![],
            locals: vec![],
        }
    }

    pub fn add_constant(&mut self, val: LiteralValue) -> usize {
        self.constants.push(val);
        self.constants.len() - 1
    }

    pub fn emit(&mut self, op: OpCode) {
        self.code.push(op);
    }

    pub fn declare_variable(&mut self, name: String) -> usize {
        self.locals.push(name);
        self.locals.len() - 1
    }

    pub fn resolve_variable(&self, name: &str) -> usize {
        self.locals
            .iter()
            .position(|s| s == name)
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
                OpCode::MulI64 => {
                    println!("MUL_I64");
                }
                OpCode::NegI64 => {
                    println!("NEG_I64");
                }
                OpCode::Return => {
                    println!("RETURN");
                }
                OpCode::Halt => {
                    println!("HALT");
                }
                OpCode::Print => {
                    println!("PRINT");
                }
                _ => {
                    println!("UNKNOWN {:?}", op);
                }
            }
        }

        println!("\nConstants:");
        for (i, c) in self.constants.iter().enumerate() {
            println!("  [{}] {:?}", i, c);
        }
    }
}
