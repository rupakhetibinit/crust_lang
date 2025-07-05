use ast::LiteralValue;

use crate::{chunk::Chunk, codegen::OpCode};

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
}

#[derive(Debug)]
pub struct VirtualMachine {
    stack: Vec<Value>,
    locals: Vec<Value>,
    pc: usize,
}

impl VirtualMachine {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            locals: Vec::new(),
            pc: 0,
        }
    }

    pub fn run(&mut self, chunk: &Chunk) {
        self.pc = 0;
        while self.pc < chunk.code.len() {
            let op = &chunk.code[self.pc];
            self.pc += 1;
            self.execute(op, chunk);
        }
    }

    fn execute(&mut self, op: &OpCode, chunk: &Chunk) {
        match op {
            OpCode::LoadConst(idx) => {
                let val = &chunk.constants[*idx];
                let val = match val {
                    LiteralValue::Int(i) => Value::Int(*i),
                    LiteralValue::Float(f) => Value::Float(*f),
                    LiteralValue::Bool(b) => Value::Bool(*b),
                    LiteralValue::RawString(s) => Value::String(s.clone()),
                };
                self.stack.push(val);
            }
            OpCode::LoadLocal(idx) => {
                let val = self.locals[*idx].clone();
                self.stack.push(val);
            }
            OpCode::StoreLocal(idx) => {
                let val = self.stack.pop().expect("stack underflow");
                if *idx < self.locals.len() {
                    self.locals[*idx] = val;
                } else {
                    self.locals.resize(*idx + 1, Value::Int(0));
                    self.locals[*idx] = val;
                }
            }
            OpCode::AddI64 => {
                let right = self.pop_int();
                let left = self.pop_int();
                self.stack.push(Value::Int(left + right));
            }
            OpCode::MulI64 => {
                let right = self.pop_int();
                let left = self.pop_int();
                self.stack.push(Value::Int(left * right));
            }
            OpCode::NegI64 => {
                let val = self.pop_int();
                self.stack.push(Value::Int(-val));
            }
            OpCode::Return => {
                let val = self.stack.pop().expect("stack underflow");
                println!("{:?}", val);
            }
            OpCode::Halt => {
                return;
            }
            _ => panic!("Unsupported opcode: {:?}", op),
        }
    }

    fn pop_int(&mut self) -> i64 {
        match self.stack.pop().expect("stack underflow") {
            Value::Int(i) => i,
            other => panic!("expected int on stack, got {:?}", other),
        }
    }
}
