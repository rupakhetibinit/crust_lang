use std::fmt::Write;

use crate::value::Value;

pub type Reg = u32;
pub type FuncId = u32;
pub type HostId = u32;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Str,
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Const {
    Int(i64),
    Float(f64),
    Bool(bool),
    Str(String),
}

impl Const {
    pub fn to_value(&self) -> Value {
        match self {
            Const::Int(value) => Value::Int(*value),
            Const::Float(value) => Value::Float(*value),
            Const::Bool(value) => Value::Bool(*value),
            Const::Str(value) => Value::Str(value.clone()),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompareOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Callable {
    Function(FuncId),
    Host(HostId),
}

#[derive(Debug, Clone)]
pub enum Inst {
    LoadConst {
        dst: Reg,
        value: Const,
    },
    Move {
        dst: Reg,
        src: Reg,
    },
    AddI {
        dst: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    AddImm {
        dst: Reg,
        lhs: Reg,
        imm: i64,
    },
    SubI {
        dst: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    SubImm {
        dst: Reg,
        lhs: Reg,
        imm: i64,
    },
    MulI {
        dst: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    DivI {
        dst: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    ModI {
        dst: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    NegI {
        dst: Reg,
        src: Reg,
    },
    AddF {
        dst: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    SubF {
        dst: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    MulF {
        dst: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    DivF {
        dst: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    NegF {
        dst: Reg,
        src: Reg,
    },
    CmpI {
        dst: Reg,
        lhs: Reg,
        rhs: Reg,
        op: CompareOp,
    },
    CmpImmI {
        dst: Reg,
        lhs: Reg,
        imm: i64,
        op: CompareOp,
    },
    CmpF {
        dst: Reg,
        lhs: Reg,
        rhs: Reg,
        op: CompareOp,
    },
    CmpBool {
        dst: Reg,
        lhs: Reg,
        rhs: Reg,
        op: CompareOp,
    },
    And {
        dst: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    Or {
        dst: Reg,
        lhs: Reg,
        rhs: Reg,
    },
    Not {
        dst: Reg,
        src: Reg,
    },
    Call {
        dst: Reg,
        target: Callable,
        args: Vec<Reg>,
    },
    Jump {
        target: usize,
    },
    JumpIfFalse {
        cond: Reg,
        target: usize,
    },
    Return {
        src: Option<Reg>,
    },
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<Type>,
    pub return_type: Type,
    pub nregs: usize,
    pub constants: Vec<Const>,
    pub code: Vec<Inst>,
}

#[derive(Debug, Clone)]
pub struct Module {
    pub functions: Vec<Function>,
    pub entry: FuncId,
    pub host_names: Vec<String>,
}

impl Module {
    pub fn disassemble(&self) -> String {
        let mut out = String::new();
        let _ = writeln!(out, "== Crust register module ==");
        let _ = writeln!(
            out,
            "entry: fn {}",
            self.functions[self.entry as usize].name
        );

        for (id, function) in self.functions.iter().enumerate() {
            let params = function
                .params
                .iter()
                .map(|ty| format!("{ty:?}"))
                .collect::<Vec<_>>()
                .join(", ");
            let _ = writeln!(
                out,
                "\n[{id}] fn {}({}) -> {:?} [{} registers]",
                function.name, params, function.return_type, function.nregs
            );
            for (pc, inst) in function.code.iter().enumerate() {
                let _ = writeln!(out, "  {pc:04} {}", format_inst(inst));
            }
            if !function.constants.is_empty() {
                let _ = writeln!(out, "  constants: {:?}", function.constants);
            }
        }

        let _ = writeln!(out, "\nhost functions: {:?}", self.host_names);
        out
    }
}

fn format_inst(inst: &Inst) -> String {
    match inst {
        Inst::LoadConst { dst, value } => format!("LOAD_CONST  r{dst}, {value:?}"),
        Inst::Move { dst, src } => format!("MOVE        r{dst}, r{src}"),
        Inst::AddI { dst, lhs, rhs } => format!("ADD_I       r{dst}, r{lhs}, r{rhs}"),
        Inst::AddImm { dst, lhs, imm } => format!("ADD_IMM     r{dst}, r{lhs}, {imm}"),
        Inst::SubI { dst, lhs, rhs } => format!("SUB_I       r{dst}, r{lhs}, r{rhs}"),
        Inst::SubImm { dst, lhs, imm } => format!("SUB_IMM     r{dst}, r{lhs}, {imm}"),
        Inst::MulI { dst, lhs, rhs } => format!("MUL_I       r{dst}, r{lhs}, r{rhs}"),
        Inst::DivI { dst, lhs, rhs } => format!("DIV_I       r{dst}, r{lhs}, r{rhs}"),
        Inst::ModI { dst, lhs, rhs } => format!("MOD_I       r{dst}, r{lhs}, r{rhs}"),
        Inst::NegI { dst, src } => format!("NEG_I       r{dst}, r{src}"),
        Inst::AddF { dst, lhs, rhs } => format!("ADD_F       r{dst}, r{lhs}, r{rhs}"),
        Inst::SubF { dst, lhs, rhs } => format!("SUB_F       r{dst}, r{lhs}, r{rhs}"),
        Inst::MulF { dst, lhs, rhs } => format!("MUL_F       r{dst}, r{lhs}, r{rhs}"),
        Inst::DivF { dst, lhs, rhs } => format!("DIV_F       r{dst}, r{lhs}, r{rhs}"),
        Inst::NegF { dst, src } => format!("NEG_F       r{dst}, r{src}"),
        Inst::CmpI { dst, lhs, rhs, op } => {
            format!("CMP_I       r{dst}, r{lhs}, r{rhs}, {op:?}")
        }
        Inst::CmpImmI { dst, lhs, imm, op } => {
            format!("CMP_IMM     r{dst}, r{lhs}, {imm}, {op:?}")
        }
        Inst::CmpF { dst, lhs, rhs, op } => {
            format!("CMP_F       r{dst}, r{lhs}, r{rhs}, {op:?}")
        }
        Inst::CmpBool { dst, lhs, rhs, op } => {
            format!("CMP_BOOL    r{dst}, r{lhs}, r{rhs}, {op:?}")
        }
        Inst::And { dst, lhs, rhs } => format!("AND         r{dst}, r{lhs}, r{rhs}"),
        Inst::Or { dst, lhs, rhs } => format!("OR          r{dst}, r{lhs}, r{rhs}"),
        Inst::Not { dst, src } => format!("NOT         r{dst}, r{src}"),
        Inst::Call { dst, target, args } => {
            let target = match target {
                Callable::Function(id) => format!("fn{id}"),
                Callable::Host(id) => format!("host{id}"),
            };
            let args = args
                .iter()
                .map(|reg| format!("r{reg}"))
                .collect::<Vec<_>>()
                .join(", ");
            format!("CALL        r{dst}, {target}({args})")
        }
        Inst::Jump { target } => format!("JUMP        {target}"),
        Inst::JumpIfFalse { cond, target } => format!("JUMP_FALSE  r{cond}, {target}"),
        Inst::Return { src } => match src {
            Some(reg) => format!("RETURN      r{reg}"),
            None => "RETURN      unit".to_string(),
        },
    }
}
