use crate::{
    chunk::Chunk,
    codegen::{ConstantType, FunctionObj, OpCode},
};

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Function(FunctionObj),
    Void, // Add a void value type
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(i) => f.write_str(&i.to_string()),
            Value::Float(fl) => f.write_str(&fl.to_string()),
            Value::Bool(b) => f.write_str(&b.to_string()),
            Value::String(s) => f.write_str(s),
            Value::Function(_) => f.write_str("<function>"),
            Value::Void => f.write_str("void"),
        }
    }
}

#[derive(Debug)]
pub struct VirtualMachine {
    stack: Vec<Value>,
    locals: Vec<Value>,
    pc: usize,
    halted: bool,
}

impl VirtualMachine {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            locals: Vec::new(),
            pc: 0,
            halted: false,
        }
    }

    pub fn run(&mut self, chunk: &Chunk) {
        // println!(
        //     "DEBUG: VM starting execution with {} instructions",
        //     chunk.code.len()
        // );
        self.pc = 0;
        self.halted = false;

        while self.pc < chunk.code.len() && !self.halted {
            let op = &chunk.code[self.pc];
            //println!("DEBUG: VM executing instruction {}: {:?}", self.pc, op);
            self.pc += 1;
            self.execute(op, chunk);
        }

        //println!("DEBUG: VM finished execution. Halted: {}", self.halted);
    }

    fn execute(&mut self, op: &OpCode, chunk: &Chunk) {
        match op {
            OpCode::LoadConst(idx) => {
                let val = &chunk.constants[*idx];
                let val = match val {
                    ConstantType::Int(i) => Value::Int(*i),
                    ConstantType::Float(f) => Value::Float(*f),
                    ConstantType::Bool(b) => Value::Bool(*b),
                    ConstantType::String(s) => Value::String(s.clone()),
                    ConstantType::Function(function_obj) => Value::Function(function_obj.clone()),
                };
                //println!("DEBUG: LoadConst pushed {:?} onto stack", val);
                self.stack.push(val);
            }
            OpCode::LoadLocal(idx) => {
                if *idx >= self.locals.len() {
                    panic!(
                        "Local variable index {} out of bounds (have {})",
                        idx,
                        self.locals.len()
                    );
                }
                let val = self.locals[*idx].clone();
                //println!("DEBUG: LoadLocal({}) pushed {:?} onto stack", idx, val);
                self.stack.push(val);
            }
            OpCode::StoreLocal(idx) => {
                let val = self.stack.pop().expect("stack underflow");
                //println!("DEBUG: StoreLocal({}) storing {:?}", idx, val);
                if *idx >= self.locals.len() {
                    self.locals.resize(*idx + 1, Value::Void);
                }
                self.locals[*idx] = val;
            }
            OpCode::AddI64 => {
                let right = self.pop_int();
                let left = self.pop_int();
                let result = left + right;
                //println!("DEBUG: AddI64: {} + {} = {}", left, right, result);
                self.stack.push(Value::Int(result));
            }
            OpCode::AddF64 => {
                let right = self.pop_float();
                let left = self.pop_float();
                let result = left + right;
                //println!("DEBUG: AddF64: {} + {} = {}", left, right, result);
                self.stack.push(Value::Float(result));
            }
            OpCode::MulI64 => {
                let right = self.pop_int();
                let left = self.pop_int();
                let result = left * right;
                //println!("DEBUG: MulI64: {} * {} = {}", left, right, result);
                self.stack.push(Value::Int(result));
            }
            OpCode::NegI64 => {
                let val = self.pop_int();
                let result = -val;
                //println!("DEBUG: NegI64: -{} = {}", val, result);
                self.stack.push(Value::Int(result));
            }
            OpCode::Return => {
                //println!("DEBUG: Return instruction executed");
                let return_value = self.stack.pop().unwrap_or(Value::Void);
                self.stack.clear(); // clear locals
                self.stack.push(return_value); // leave return value on stack
                self.halted = true;
            }
            OpCode::Halt => {
                //println!("DEBUG: Halt instruction executed");
                self.halted = true;
            }
            OpCode::Print => {
                let val = self.stack.pop().expect("Stack underflow");
                //println!("DEBUG: Print instruction - printing: {:?}", val);

                // Print the value
                match val {
                    Value::Function(func_obj) => {
                        //println!("DEBUG: Cannot print function directly");
                        println!("<function: {}>", func_obj.name);
                    }
                    _ => {
                        println!("{}", val);
                    }
                }
            }
            OpCode::Call(arity, idx) => {
                // println!(
                //     "DEBUG: Call instruction - arity: {}, function index: {}",
                //     arity, idx
                // );

                // Check if we have enough arguments on the stack
                if self.stack.len() < *arity as usize {
                    panic!(
                        "Not enough arguments on stack for function call. Expected {}, got {}",
                        arity,
                        self.stack.len()
                    );
                }

                // Collect arguments
                let mut args = Vec::with_capacity(*arity as usize);
                for _ in 0..*arity {
                    let arg = self.stack.pop().expect("stack underflow");
                    //println!("DEBUG: Collected argument {}: {:?}", i, arg);
                    args.push(arg);
                }
                args.reverse(); // Arguments are pushed in reverse order

                // Get the function
                let function = &chunk.constants[*idx];
                if let ConstantType::Function(func_obj) = function {
                    //println!("DEBUG: Calling function: {}", func_obj.name);

                    // Create a new VM for the function call
                    let mut function_vm = VirtualMachine::new();

                    // Set up the function's local variables with the arguments
                    function_vm
                        .locals
                        .resize(func_obj.arity.max(1), Value::Void);
                    for (i, arg) in args.into_iter().enumerate() {
                        if i < func_obj.arity {
                            function_vm.locals[i] = arg;
                        }
                    }

                    // Execute the function
                    function_vm.run(&func_obj.chunk);

                    // If the function returned a value, push it onto our stack
                    if !function_vm.stack.is_empty() {
                        let return_value = function_vm.stack.last().unwrap().clone();
                        //println!("DEBUG: Function returned: {:?}", return_value);
                        self.stack.push(return_value);
                    } else {
                        //println!("DEBUG: Function returned void");
                        // For void functions, don't push anything
                    }
                } else {
                    panic!("Expected a function constant at index {}", idx);
                }
            }
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
            Value::Float(f) => f,
            other => panic!("expected float on stack, got {:?}", other),
        }
    }
}
