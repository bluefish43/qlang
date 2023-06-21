use fxhash::FxHashMap;

use crate::{function::Function, vm::{Value, Types}};

#[derive(Clone, PartialEq, Debug)]
pub struct Class {
    pub name: String,
    pub public_methods: FxHashMap<String, Function>,
    pub private_methods: FxHashMap<String, Function>,
    pub static_methods: FxHashMap<String, Function>,
    pub public_properties: FxHashMap<String, (Value, Types)>,
    pub private_properties: FxHashMap<String, (Value, Types)>,
}
