use crate::{chunk::Chunk, codegen::OpCode};
use crust_frontend::ast::LiteralValue;

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(i) => f.write_str(&i.to_string()),
            Value::Float(fl) => f.write_str(&fl.to_string()),
            Value::Bool(b) => f.write_str(&b.to_string()),
            Value::String(s) => f.write_str(s),
        }
    }
}

#[derive(Debug)]
pub struct VirtualMachine {
    stack: Vec<Value>,
    pc: usize,
    frames: Vec<Frame>,
    current_base: usize,
}

#[derive(Debug)]
struct Frame {
    return_pc: usize,
    caller_base: usize,
    callee_base: usize,
}

impl VirtualMachine {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            pc: 0,
            frames: Vec::new(),
            current_base: 0,
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
                let val = self.stack[self.current_base + *idx].clone();
                self.stack.push(val);
            }
            OpCode::StoreLocal(idx) => {
                let val = self.stack.pop().expect("stack underflow");
                let slot = self.current_base + *idx;
                if slot < self.stack.len() {
                    self.stack[slot] = val;
                } else {
                    self.stack.resize(slot + 1, Value::Int(0));
                    self.stack[slot] = val;
                }
            }
            OpCode::AddI64 => {
                let right = self.pop_int();
                let left = self.pop_int();
                self.stack.push(Value::Int(left + right));
            }
            OpCode::AddF64 => {
                let right = self.pop_float();
                let left = self.pop_float();
                self.stack.push(Value::Float(left + right));
            }
            OpCode::SubI64 => {
                let right = self.pop_int();
                let left = self.pop_int();
                self.stack.push(Value::Int(left - right));
            }
            OpCode::EqualI64 => self.compare_int(|left, right| left == right),
            OpCode::LessI64 => self.compare_int(|left, right| left < right),
            OpCode::LessEqualI64 => self.compare_int(|left, right| left <= right),
            OpCode::GreaterI64 => self.compare_int(|left, right| left > right),
            OpCode::GreaterEqualI64 => self.compare_int(|left, right| left >= right),
            OpCode::MulI64 => {
                let right = self.pop_int();
                let left = self.pop_int();
                self.stack.push(Value::Int(left * right));
            }
            OpCode::MulF64 => {
                let right = self.pop_float();
                let left = self.pop_float();
                self.stack.push(Value::Float(left * right));
            }
            OpCode::NegI64 => {
                let val = self.pop_int();
                self.stack.push(Value::Int(-val));
            }
            OpCode::Return => {
                let val = self.stack.pop().expect("stack underflow");
                if let Some(frame) = self.frames.pop() {
                    self.stack.truncate(frame.callee_base);
                    self.current_base = frame.caller_base;
                    self.pc = frame.return_pc;
                    self.stack.push(val);
                } else {
                    println!("Program Returned {:?}", val);
                    self.pc = chunk.code.len();
                }
            }
            OpCode::Halt => {
                self.pc = chunk.code.len();
            }
            OpCode::Print(argument_count) => {
                let mut arguments = self.stack.split_off(
                    self.stack
                        .len()
                        .checked_sub(*argument_count)
                        .expect("stack underflow"),
                );
                let format = arguments
                    .first()
                    .expect("print requires at least one argument")
                    .to_string();
                let mut output = format;
                for argument in arguments.drain(1..) {
                    output = output.replacen("{}", &argument.to_string(), 1);
                }
                println!("{}", output);
            }
            OpCode::DivF64 => {
                let left = self.pop_float();
                let right = self.pop_float();
                self.stack.push(Value::Float(left as f64 / right as f64));
            }
            OpCode::DivI64 => {
                let right = self.pop_int();
                let left = self.pop_int();
                self.stack.push(Value::Int(left / right));
            }
            OpCode::IncrementLocal(idx) => {
                let value = self.local_int(*idx);
                self.store_local(*idx, Value::Int(value + 1));
            }
            OpCode::DecrementLocal(idx) => {
                let value = self.local_int(*idx);
                self.store_local(*idx, Value::Int(value - 1));
            }
            OpCode::JumpIfFalse(target) => {
                if !self.pop_bool() {
                    self.pc = *target;
                }
            }
            OpCode::Jump(target) => self.pc = *target,
            OpCode::Call(target, argument_count) => {
                let callee_base = self
                    .stack
                    .len()
                    .checked_sub(*argument_count)
                    .expect("stack underflow");
                self.frames.push(Frame {
                    return_pc: self.pc,
                    caller_base: self.current_base,
                    callee_base,
                });
                self.current_base = callee_base;
                self.pc = *target;
            }
        }
    }

    fn compare_int(&mut self, compare: impl FnOnce(i64, i64) -> bool) {
        let right = self.pop_int();
        let left = self.pop_int();
        self.stack.push(Value::Bool(compare(left, right)));
    }

    fn local_int(&self, idx: usize) -> i64 {
        match self
            .stack
            .get(self.current_base + idx)
            .expect("invalid local slot")
        {
            Value::Int(value) => *value,
            other => panic!("expected integer local, got {:?}", other),
        }
    }

    fn store_local(&mut self, idx: usize, value: Value) {
        let slot = self.current_base + idx;
        if slot < self.stack.len() {
            self.stack[slot] = value;
        } else {
            self.stack.resize(slot + 1, Value::Int(0));
            self.stack[slot] = value;
        }
    }

    fn pop_int(&mut self) -> i64 {
        match self.stack.pop().expect("stack underflow") {
            Value::Int(i) => i,
            other => panic!("expected int on stack, got {:?}", other),
        }
    }

    fn pop_float(&mut self) -> f64 {
        match self.stack.pop().expect("stack underflow") {
            Value::Float(i) => i,
            other => panic!("expected float on stack, got {:?}", other),
        }
    }

    #[allow(unused)]
    fn pop_string(&mut self) -> String {
        match self.stack.pop().expect("stack underflow") {
            Value::String(i) => i,
            other => panic!("expected float on stack, got {:?}", other),
        }
    }

    fn pop_bool(&mut self) -> bool {
        match self.stack.pop().expect("stack underflow") {
            Value::Bool(i) => i,
            other => panic!("expected float on stack, got {:?}", other),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Value, VirtualMachine};
    use crate::{chunk::Chunk, codegen::OpCode};
    use crust_frontend::ast::LiteralValue;

    #[test]
    fn executes_for_loop_bytecode() {
        let mut chunk = Chunk::new();
        let zero = chunk.add_constant(LiteralValue::Int(0));
        let three = chunk.add_constant(LiteralValue::Int(3));
        chunk.emit(OpCode::LoadConst(zero));
        chunk.emit(OpCode::StoreLocal(0));
        let condition = chunk.code.len();
        chunk.emit(OpCode::LoadLocal(0));
        chunk.emit(OpCode::LoadConst(three));
        chunk.emit(OpCode::LessI64);
        let exit = chunk.code.len();
        chunk.emit(OpCode::JumpIfFalse(usize::MAX));
        chunk.emit(OpCode::IncrementLocal(0));
        chunk.emit(OpCode::Jump(condition));
        chunk.patch_jump(exit, chunk.code.len());
        chunk.emit(OpCode::Halt);

        let mut vm = VirtualMachine::new();
        vm.run(&chunk);

        assert!(matches!(vm.stack[0], Value::Int(3)));
    }
}
