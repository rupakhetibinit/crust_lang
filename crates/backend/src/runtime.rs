use std::sync::Arc;

use crust_frontend::typed::HostFnDecl;

use crate::value::Value;

pub type HostCall = dyn Fn(&[Value]) -> Value + Send + Sync;

#[derive(Clone)]
pub struct HostFn {
    pub decl: HostFnDecl,
    pub call: Arc<HostCall>,
}

impl HostFn {
    pub fn new<F>(decl: HostFnDecl, call: F) -> Self
    where
        F: Fn(&[Value]) -> Value + Send + Sync + 'static,
    {
        Self {
            decl,
            call: Arc::new(call),
        }
    }
}
