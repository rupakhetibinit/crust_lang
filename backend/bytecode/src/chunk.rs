use crate::codegen::{ConstantType, OpCode};

#[derive(Debug, Clone)]
pub struct Chunk {
    pub code: Vec<OpCode>,
    pub constants: Vec<ConstantType>,
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

    pub fn add_constant(&mut self, val: ConstantType) -> usize {
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
        self.disassemble_named("<main>", 0);
    }

    /// Internal workhorse: recursive with indentation.
    fn disassemble_named(&self, where_am_i: &str, depth: usize) {
        let indent = "  ".repeat(depth);
        println!("{indent}== Chunk {where_am_i} ==");

        // 1⃣ opcodes
        for (i, op) in self.code.iter().enumerate() {
            print!("{indent}{:04} ", i);
            match op {
                OpCode::LoadConst(idx) => println!("LOAD_CONST   {}", idx),
                OpCode::LoadLocal(idx) => println!("LOAD_LOCAL   {}", idx),
                OpCode::StoreLocal(idx) => println!("STORE_LOCAL  {}", idx),
                OpCode::AddI64 => println!("ADD_I64"),
                OpCode::MulI64 => println!("MUL_I64"),
                OpCode::NegI64 => println!("NEG_I64"),
                OpCode::Return => println!("RETURN"),
                OpCode::Halt => println!("HALT"),
                OpCode::Print => println!("PRINT"),
                OpCode::Call(arity, idx) => {
                    let name = self.function_name(*idx).unwrap_or("<unknown>");
                    println!("CALL         {:>3} {:>3}  ; {}", idx, arity, name);
                }
                OpCode::AddF64 => println!("ADD_F64"),
                OpCode::MulF64 => println!("MUL_F64"),
            }
        }

        // 2⃣ constants
        println!("\n{indent}Constants:");
        for (i, c) in self.constants.iter().enumerate() {
            match c {
                ConstantType::Function(f) => {
                    println!("{indent}{:04}: <fn {} arity {}>", i, f.name, f.arity);
                    // recurse – one level deeper
                    f.chunk.disassemble_named(&f.name, depth + 1);
                }
                other => println!("{indent}{:04}: {:?}", i, other),
            }
        }
        if depth == 0 {
            println!();
        } // extra blank line at end of top‑level dump
    }

    /// tiny helper so the match above stays tidy
    fn function_name(&self, idx: usize) -> Option<&str> {
        match self.constants.get(idx) {
            Some(ConstantType::Function(f)) => Some(&f.name),
            _ => None,
        }
    }
}
