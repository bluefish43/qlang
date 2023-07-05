use fxhash::FxHashMap;

use crate::{function::Function, vm::Value};

#[derive(Clone, PartialEq)]
pub struct Class {
    pub name: String,
    pub staticmethods: FxHashMap<String, Function>,
    pub properties: FxHashMap<String, Value>,
}
