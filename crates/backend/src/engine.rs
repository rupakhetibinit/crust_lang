use std::{
    fs,
    sync::{Arc, Mutex},
};

use crust_frontend::{
    Parser,
    lexer::Lexer,
    typechecker::{TypeChecker, TypeError},
    typed::HostFnDecl,
};
use thiserror::Error;

use crate::{
    codegen::CodeGen,
    interpreter::{Interpreter, RuntimeError},
    ir::{FuncId, HostId, Module},
    jit::{self, JitState},
    runtime::HostFn,
    value::Value,
};

#[derive(Debug, Error)]
pub enum Error {
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),
    #[error("parse error:\n{0}")]
    Parse(String),
    #[error("type checking failed: {0:?}")]
    Type(Vec<TypeError>),
    #[error("runtime error: {0}")]
    Runtime(#[from] RuntimeError),
    #[error("JIT error: {0}")]
    Jit(#[from] jit::JitError),
    #[error("host function '{0}' is already registered")]
    DuplicateHost(String),
    #[error("unknown function '{0}'")]
    UnknownFunction(String),
}

pub struct Engine {
    hosts: Vec<HostFn>,
    jit: Option<Arc<Mutex<JitState>>>,
}

impl Default for Engine {
    fn default() -> Self {
        Self::new()
    }
}

impl Engine {
    pub fn new() -> Self {
        Self::with_output(|line| println!("{line}"))
    }

    pub fn with_output<F>(output: F) -> Self
    where
        F: Fn(&str) + Send + Sync + 'static,
    {
        let output: Arc<dyn Fn(&str) + Send + Sync> = Arc::new(output);
        let mut hosts = Vec::new();

        let print_output = Arc::clone(&output);
        let print_decl = HostFnDecl {
            name: "print".to_string(),
            params: vec![crust_frontend::typed::Type::Any],
            variadic: true,
            return_type: crust_frontend::typed::Type::Void,
        };
        hosts.push(HostFn::new(print_decl, move |args| {
            let mut line = args.first().map(ToString::to_string).unwrap_or_default();
            for arg in &args[1..] {
                line = line.replacen("{}", &arg.to_string(), 1);
            }
            print_output(&line);
            Value::Unit
        }));

        Self::with_jit_threshold(hosts, 64)
    }

    fn with_jit_threshold(hosts: Vec<HostFn>, threshold: u32) -> Self {
        let jit = JitState::new(threshold, true)
            .ok()
            .map(|jit| Arc::new(Mutex::new(jit)));
        Self { hosts, jit }
    }

    pub fn set_jit_threshold(&mut self, threshold: u32) {
        self.jit = JitState::new(threshold, true)
            .ok()
            .map(|jit| Arc::new(Mutex::new(jit)));
    }

    pub fn jit_enabled(&self) -> bool {
        self.jit.is_some()
    }

    pub fn register_host<F>(&mut self, decl: HostFnDecl, call: F) -> Result<HostId, Error>
    where
        F: Fn(&[Value]) -> Value + Send + Sync + 'static,
    {
        if self.hosts.iter().any(|host| host.decl.name == decl.name) {
            return Err(Error::DuplicateHost(decl.name));
        }
        let id = self.hosts.len() as HostId;
        self.hosts.push(HostFn::new(decl, call));
        Ok(id)
    }

    pub fn compile(&self, source: &str) -> Result<Module, Error> {
        let lexer = Lexer::new(source);
        let mut parser = Parser::new(&lexer.tokens, source);
        let (root_id, arena) = parser
            .parse()
            .map_err(|error| Error::Parse(error.display_with_source(source, None)))?;

        let host_decls: Vec<HostFnDecl> = self.hosts.iter().map(|host| host.decl.clone()).collect();
        let mut checker = TypeChecker::with_hosts(arena, host_decls.clone());
        let (typed_root_id, typed_arena) = checker.type_check(root_id).map_err(Error::Type)?;

        let codegen = CodeGen::new(typed_arena, host_decls);
        Ok(codegen.generate(typed_root_id))
    }

    pub fn execute(&self, module: &Module) -> Result<Value, Error> {
        self.call_function(module, module.entry, &[])
    }

    pub fn call(&self, module: &Module, name: &str, args: &[Value]) -> Result<Value, Error> {
        let func_id = module
            .functions
            .iter()
            .position(|function| function.name == name)
            .ok_or_else(|| Error::UnknownFunction(name.to_string()))?
            as FuncId;
        self.call_function(module, func_id, args)
    }

    fn call_function(
        &self,
        module: &Module,
        func_id: FuncId,
        args: &[Value],
    ) -> Result<Value, Error> {
        if let Some(jit) = &self.jit {
            let bridge = jit::JitBridge::new(Arc::clone(jit), module, &self.hosts);
            {
                let mut state = jit.lock().unwrap();
                if state.should_compile(module, func_id) {
                    state.compile_function(module, func_id)?;
                }
            }
            if let Some(value) = bridge.call_if_compiled(func_id, args) {
                return Ok(value);
            }
            let mut interpreter = Interpreter::new(module, &self.hosts, Some(&bridge));
            return Ok(interpreter.call(func_id, args.to_vec())?);
        }

        let mut interpreter = Interpreter::new(module, &self.hosts, None);
        Ok(interpreter.call(func_id, args.to_vec())?)
    }

    pub fn run(&self, source: &str) -> Result<Value, Error> {
        let module = self.compile(source)?;
        self.execute(&module)
    }

    pub fn run_file(&self, path: &str) -> Result<Value, Error> {
        let source = fs::read_to_string(path)?;
        self.run(&source)
    }

    pub fn disassemble(&self, source: &str) -> Result<String, Error> {
        Ok(self.compile(source)?.disassemble())
    }

    pub fn disassemble_file(&self, path: &str) -> Result<String, Error> {
        let source = fs::read_to_string(path)?;
        self.disassemble(&source)
    }
}
