use fxhash::FxHashMap;

use super::{
    parser::{QoTypes}
};

pub fn return_function_data() -> FxHashMap<String, (Option<Vec<(String, Vec<QoTypes>)>>, QoTypes)> {
    let mut map = FxHashMap::default();

    map.insert(String::from("fmt.print"), (None, QoTypes::None));
    map.insert(String::from("fmt.println"), (None, QoTypes::None));
    map.insert(String::from("io.read_line"), (Some(vec![]), QoTypes::String));
    map.insert(String::from("concatenate"), (None, QoTypes::String));
    map.insert(String::from("string.to_upper"), (Some(vec![(String::from("string"), vec![QoTypes::String])]), QoTypes::String));
    map.insert(String::from("string.to_lower"), (Some(vec![(String::from("string"), vec![QoTypes::String])]), QoTypes::String));
    map.insert(String::from("string.is_numeric"), (Some(vec![(String::from("string"), vec![QoTypes::String])]), QoTypes::Bool));
    map.insert(String::from("string.is_lower"), (Some(vec![(String::from("string"), vec![QoTypes::String])]), QoTypes::Bool));
    map.insert(String::from("string.is_empty"), (Some(vec![(String::from("string"), vec![QoTypes::String])]), QoTypes::Bool));
    map.insert(String::from("string.is_upper"), (Some(vec![(String::from("string"), vec![QoTypes::String])]), QoTypes::Bool));
    map.insert(String::from("string.trim"), (Some(vec![(String::from("string"), vec![QoTypes::String])]), QoTypes::String));
    map.insert(String::from("string.is_hex"), (Some(vec![(String::from("string"), vec![QoTypes::String])]), QoTypes::Bool));
    map.insert(String::from("string.is_octal"), (Some(vec![(String::from("string"), vec![QoTypes::String])]), QoTypes::Bool));
    map.insert(String::from("string.is_binary"), (Some(vec![(String::from("string"), vec![QoTypes::String])]), QoTypes::Bool));
    map.insert(String::from("string.length"), (Some(vec![(String::from("string"), vec![QoTypes::String])]), QoTypes::I32));
    map.insert(String::from("string.count"), (Some(vec![(String::from("string"), vec![QoTypes::String]), (String::from("substring"), vec![QoTypes::String])]), QoTypes::I32));
    map.insert(String::from("string.starts_with"), (Some(vec![(String::from("string"), vec![QoTypes::String]), (String::from("substring"), vec![QoTypes::String])]), QoTypes::Bool));
    map.insert(String::from("string.ends_with"), (Some(vec![(String::from("string"), vec![QoTypes::String]), (String::from("substring"), vec![QoTypes::String])]), QoTypes::Bool));
    map.insert(String::from("string.replace"), (Some(vec![(String::from("string"), vec![QoTypes::String]), (String::from("old"), vec![QoTypes::String]), (String::from("new"), vec![QoTypes::String])]), QoTypes::String));
    map.insert(String::from("string.substring"), (Some(vec![(String::from("string"), vec![QoTypes::String]), (String::from("start"), vec![QoTypes::I32]), (String::from("end"), vec![QoTypes::I32])]), QoTypes::String));
    map.insert(String::from("list.join"), (Some(vec![(String::from("_list"), vec![QoTypes::List]), (String::from("separator"), vec![QoTypes::String])]), QoTypes::String));
    map.insert(String::from("string.split"), (Some(vec![(String::from("string"), vec![QoTypes::String]), (String::from("separator"), vec![QoTypes::String])]), QoTypes::List));
    map.insert(String::from("string.lines"), (Some(vec![(String::from("string"), vec![QoTypes::String])]), QoTypes::List));
    map.insert(String::from("string.from"), (Some(vec![(String::from("value"), vec![QoTypes::Any])]), QoTypes::String));
    map.insert(String::from("pointer.read"), (Some(vec![(String::from("pointer"), vec![QoTypes::Pointer])]), QoTypes::Any));
    map.insert(String::from("pointer.read"), (Some(vec![(String::from("pointer"), vec![QoTypes::Pointer]), (String::from("value"), vec![QoTypes::Any])]), QoTypes::None));
    map.insert(String::from("pointer.copy"), (Some(vec![(String::from("pointer"), vec![QoTypes::Pointer])]), QoTypes::Any));
    map.insert(String::from("pointer.from"), (Some(vec![(String::from("value"), vec![QoTypes::Any])]), QoTypes::Pointer));
    map.insert(String::from("process.exit"), (Some(vec![(String::from("exit_code"), vec![QoTypes::I32])]), QoTypes::None));
    map.insert(String::from("list.get"), (Some(vec![(String::from("pointer"), vec![QoTypes::Pointer])]), QoTypes::Any));
    map.insert(String::from("list.push"), (Some(vec![(String::from("pointer"), vec![QoTypes::Pointer]), (String::from("value"), vec![QoTypes::Any])]), QoTypes::None));
    map.insert(String::from("list.contains"), (Some(vec![(String::from("pointer"), vec![QoTypes::Pointer]), (String::from("value"), vec![QoTypes::Any])]), QoTypes::Bool));
    map.insert(String::from("list.insert"), (Some(vec![(String::from("pointer"), vec![QoTypes::Pointer]), (String::from("position"), vec![QoTypes::I32]), (String::from("value"), vec![QoTypes::Any])]), QoTypes::None));
    map.insert(String::from("list.length"), (Some(vec![(String::from("pointer"), vec![QoTypes::Pointer])]), QoTypes::I32));
    map.insert(String::from("list.find"), (Some(vec![(String::from("pointer"), vec![QoTypes::Pointer]), (String::from("value"), vec![QoTypes::Any])]), QoTypes::I32));
    map.insert(String::from("list.first"), (Some(vec![(String::from("pointer"), vec![QoTypes::Pointer])]), QoTypes::Pointer));
    map.insert(String::from("list.last"), (Some(vec![(String::from("pointer"), vec![QoTypes::Pointer])]), QoTypes::Pointer));
    map.insert(String::from("reverse"), (Some(vec![(String::from("data"), vec![QoTypes::String, QoTypes::List])]), QoTypes::Any));
    map.insert(String::from("list.zip"), (Some(vec![(String::from("pointer"), vec![QoTypes::Pointer]), (String::from("pointer2"), vec![QoTypes::Pointer])]), QoTypes::Pointer));
    map.insert(String::from("list.enumerate"), (Some(vec![(String::from("pointer"), vec![QoTypes::Pointer])]), QoTypes::List));

    return map
}