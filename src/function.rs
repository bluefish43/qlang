use std::fmt::Debug;
use std::io::stdin;
use std::io::{Read, Write};
use std::sync::Arc;
use std::time::Duration;

use ansi_term::Color;

use chrono::prelude::*;

use crate::{
    vm::{value_to_readable, value_to_string, Instruction, Types, Value},
    get_current_time
};

#[derive(Clone)]
pub enum Function {
    Native(Arc<dyn Fn(&[Value]) -> Value>),
    Closure(FunctionStructNoName),
    Interpreted(FunctionStruct),
}

#[allow(unused)]
macro_rules! my_format {
    ($format_string:expr) => {
        format!($format_string)
    };
    ($format_string:expr, $(.$option:ident $param:expr),*) => {{
        let mut formatted_string = String::from($format_string);
        $(
            let value_str = &$param.to_string();
            match stringify!($option) {
                "f" => {
                    if let Ok(num) = value_str.parse::<usize>() {
                        formatted_string = format!("{:.1$}", formatted_string, num);
                    }
                },
                "b" => {
                    match value_str.as_str() {
                        "d" => formatted_string = format!("{:b}", formatted_string),
                        "h" => formatted_string = format!("{:x}", formatted_string),
                        "o" => formatted_string = format!("{:o}", formatted_string),
                        _ => {},
                    }
                },
                "p" => {
                    let ptr_value = format!("{:p}", &formatted_string);
                    formatted_string = formatted_string.replace(&format!("{{.{}{}}}", $option, $param), &ptr_value);
                },
                "neg" => {
                    let negated_value = format!("{}", !formatted_string);
                    formatted_string = formatted_string.replace(&format!("{{.{}{}}}", $option, $param), &negated_value);
                },
                "len" => {
                    if let Ok(len) = value_str.parse::<usize>() {
                        if let Some(value_substr) = formatted_string.get(..len) {
                            formatted_string = String::from(value_substr.trim());
                        } else {
                            formatted_string = String::new();
                        }
                    }
                },
                "punct" => {
                    let punctuated_value = formatted_string.chars().collect::<Vec<char>>().join($param);
                    formatted_string = punctuated_value;
                },
                "upper" => {
                    formatted_string = formatted_string.to_uppercase();
                },
                "lower" => {
                    formatted_string = formatted_string.to_lowercase();
                },
                "title" => {
                    formatted_string = formatted_string.chars().enumerate()
                        .map(|(i, c)| {
                            if i == 0 || formatted_string.chars().nth(i - 1).unwrap().is_whitespace() {
                                c.to_uppercase().next().unwrap()
                            } else {
                                c
                            }
                        })
                        .collect();
                },
                "reverse" => {
                    formatted_string = formatted_string.chars().rev().collect();
                },
                "replace" => {
                    if let Some(replace_str) = value_str.split(',').nth(0) {
                        if let Some(with_str) = value_str.split(',').nth(1) {
                            formatted_string = formatted_string.replace(replace_str, with_str);
                        }
                    }
                },
                _ => {
                    formatted_string = formatted_string.replace(&format!("{{.{}{}}}", $option, $param), value_str);
                },
            }
        )*
        formatted_string
    }};
}

impl PartialEq for Function {
    fn eq(&self, other: &Self) -> bool {
        match self {
            Function::Native(_) => false,
            Function::Interpreted(s) => match other {
                Function::Interpreted(s2) => s == s2,
                _ => false,
            },
            Function::Closure(_) => false,
        }
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Function::Native(_) => write!(f, "<builtin_function>"),
            Function::Interpreted(s) => write!(f, "Interpreted({:?})", s),
            Function::Closure(c) => write!(f, "closure {:?}", c),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub struct FunctionStructNoName {
    pub args: Vec<(String, Types)>,
    pub returns: Types,
    pub body: Vec<Instruction>,
}

#[derive(PartialEq, Debug, Clone)]
pub struct FunctionStruct {
    pub name: String,
    pub args: Vec<(String, Types)>,
    pub returns: Types,
    pub body: Vec<Instruction>,
}

pub fn get_natives() -> Vec<(String, Function)> {
    let mut natives: Vec<(String, Function)> = Vec::new();
    natives.push((
        String::from("print"),
        Function::Native(Arc::new(|args| {
            if args.is_empty() {
                Value::Error(String::from("print requires at least one argument"))
            } else {
                print!("{}", value_to_string(&args[0]));
                Value::None
            }
        })),
    ));
    natives.push((
        String::from("println"),
        Function::Native(Arc::new(|args| {
            if args.is_empty() {
                Value::Error(String::from("println requires at least one argument"))
            } else {
                println!("{}", value_to_string(&args[0]));
                Value::None
            }
        })),
    ));
    natives.push((
        String::from("read_line"),
        Function::Native(Arc::new(|_| {
            let mut input = String::new();
            let result = stdin().read_line(&mut input);
            if let Err(err) = result {
                Value::Error(err.to_string())
            } else {
                Value::String(input)
            }
        })),
    ));
    natives.push((
        String::from("concat"),
        Function::Native(Arc::new(|args| {
            if args.is_empty() {
                Value::Error(String::from("concat requires at least one argument"))
            } else {
                let mut result = String::new();
                for val in args {
                    result.push_str(&value_to_string(val));
                }
                Value::String(result)
            }
        })),
    ));
    natives.push((
        String::from("to_upper"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("to_upper requires exactly one argument"))
            } else {
                Value::String(value_to_string(&args[0]).to_uppercase())
            }
        })),
    ));
    natives.push((
        String::from("to_lower"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("to_lower requires exactly one argument"))
            } else {
                Value::String(value_to_string(&args[0]).to_lowercase())
            }
        })),
    ));
    natives.push((
        String::from("is_numeric"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("is_numeric requires exactly one argument"))
            } else {
                return Value::Boolean(
                    value_to_string(&args[0])
                        .chars()
                        .all(|c| c.is_ascii_digit()),
                );
            }
        })),
    ));
    natives.push((
        String::from("is_upper"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("is_upper requires exactly one argument"))
            } else {
                return Value::Boolean(value_to_string(&args[0]).chars().all(|c| c.is_uppercase()));
            }
        })),
    ));
    natives.push((
        String::from("is_lower"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("is_lower requires exactly one argument"))
            } else {
                return Value::Boolean(value_to_string(&args[0]).chars().all(|c| c.is_lowercase()));
            }
        })),
    ));
    natives.push((
        String::from("str_is_empty"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("is_empty requires exactly one argument"))
            } else {
                return Value::Boolean(value_to_string(&args[0]).is_empty());
            }
        })),
    ));
    natives.push((
        String::from("trim"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("is_empty requires exactly one argument"))
            } else {
                return Value::String(value_to_string(&args[0]).trim().to_string());
            }
        })),
    ));
    natives.push((
        String::from("is_hex"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("is_hex requires exactly one argument"))
            } else {
                return Value::Boolean(
                    value_to_string(&args[0])
                        .chars()
                        .all(|c| c.is_ascii_hexdigit()),
                );
            }
        })),
    ));
    natives.push((
        String::from("is_octal"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("is_octal requires exactly one argument"))
            } else {
                return Value::Boolean(value_to_string(&args[0]).chars().all(|c| c.is_digit(8)));
            }
        })),
    ));
    natives.push((
        String::from("is_binary"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("is_binary requires exactly one argument"))
            } else {
                return Value::Boolean(value_to_string(&args[0]).chars().all(|c| c.is_digit(2)));
            }
        })),
    ));
    natives.push((
        String::from("strlen"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("strlen requires exactly one argument"))
            } else {
                Value::Int(value_to_string(&args[0]).len() as i32)
            }
        })),
    ));
    // count: Counts the number of occurrences of a substring within a string.
    natives.push((
        String::from("count"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("count requires exactly two arguments"));
            }
            match (&args[0], &args[1]) {
                (Value::String(s), Value::String(sub)) => {
                    let count = s.matches(sub).count();
                    Value::Int(count as i32)
                }
                _ => Value::Error(String::from("count requires two string arguments")),
            }
        })),
    ));

    // starts_with: Checks if a string starts with a specific prefix.
    natives.push((
        String::from("starts_with"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("starts_with requires exactly two arguments"));
            }
            match (&args[0], &args[1]) {
                (Value::String(s), Value::String(prefix)) => {
                    let starts_with = s.starts_with(prefix);
                    Value::Boolean(starts_with)
                }
                _ => Value::Error(String::from("starts_with requires two string arguments")),
            }
        })),
    ));

    // ends_with: Checks if a string ends with a specific suffix.
    natives.push((
        String::from("ends_with"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("ends_with requires exactly two arguments"));
            }
            match (&args[0], &args[1]) {
                (Value::String(s), Value::String(suffix)) => {
                    let ends_with = s.ends_with(suffix);
                    Value::Boolean(ends_with)
                }
                _ => Value::Error(String::from("ends_with requires two string arguments")),
            }
        })),
    ));

    // replace: Replaces all occurrences of a substring within a string with another substring.
    natives.push((
        String::from("replace"),
        Function::Native(Arc::new(|args| {
            if args.len() != 3 {
                return Value::Error(String::from("replace requires exactly three arguments"));
            }
            match (&args[0], &args[1], &args[2]) {
                (Value::String(s), Value::String(old), Value::String(new)) => {
                    let replaced = s.replace(old, new);
                    Value::String(replaced)
                }
                _ => Value::Error(String::from("replace requires three string arguments")),
            }
        })),
    ));

    // substring: Retrieves a substring from a given string based on start and end indices.
    natives.push((
        String::from("substring"),
        Function::Native(Arc::new(|args| {
            if args.len() != 3 {
                return Value::Error(String::from("substring requires exactly three arguments"));
            }
            match (&args[0], &args[1], &args[2]) {
                (Value::String(s), Value::Int(start), Value::Int(end)) => {
                    let start = *start.max(&0) as usize;
                    let end = *end.max(&0) as usize;
                    if start <= end && end <= s.len() {
                        let substring = &s[start..end];
                        Value::String(String::from(substring))
                    } else {
                        Value::Error(String::from("Invalid start or end indices"))
                    }
                }
                _ => Value::Error(String::from(
                    "substring requires a string and two integer arguments",
                )),
            }
        })),
    ));

    // join: Joins elements of a list into a single string using a delimiter.
    natives.push((
        String::from("join"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("join requires exactly two arguments"));
            }
            match (&args[0], &args[1]) {
                (Value::List(list), Value::String(delimiter)) => {
                    let joined = list
                        .iter()
                        .map(|value| value.to_string())
                        .collect::<Vec<String>>()
                        .join(delimiter);
                    Value::String(joined)
                }
                _ => Value::Error(String::from("join requires a list and a string argument")),
            }
        })),
    ));

    // split: Splits a string into a list of substrings based on a delimiter.
    natives.push((
        String::from("split"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("split requires exactly two arguments"));
            }
            match (&args[0], &args[1]) {
                (Value::String(s), Value::String(delimiter)) => {
                    let substrings: Vec<Value> = s
                        .split(delimiter)
                        .map(|substring| Value::String(String::from(substring)))
                        .collect();
                    Value::List(substrings)
                }
                _ => Value::Error(String::from(
                    "split requires a string and a string delimiter",
                )),
            }
        })),
    ));

    natives.push((
        String::from("lines"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("lines requires exactly one argument"));
            }
            match &args[0] {
                Value::String(s) => {
                    let substrings: Vec<Value> = s
                        .split('\n')
                        .map(|substring| Value::String(String::from(substring)))
                        .collect();
                    Value::List(substrings)
                }
                _ => Value::Error(String::from("lines requires a string")),
            }
        })),
    ));

    natives.push((
        String::from("to_string"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("to_string requires exactly one argument"));
            }
            Value::String(value_to_string(&args[0]))
        })),
    ));

    natives.push((
        String::from("command"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("command requires exactly one argument"))
            } else {
                let binding = value_to_string(&args[0]);
                let mut commands = binding.split(' ').collect::<Vec<&str>>();
                let output = std::process::Command::new(commands.pop().unwrap())
                    .args(commands)
                    .spawn();
                match output {
                    Ok(mut child) => {
                        let output = child.wait();
                        match output {
                            Ok(exitcode) => Value::Int(exitcode.code().unwrap()),
                            Err(err) => Value::Error(err.to_string()),
                        }
                    }
                    Err(err) => Value::Error(err.to_string()),
                }
            }
        })),
    ));
    natives.push((
        String::from("command_async"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("command requires exactly one argument"))
            } else {
                let binding = value_to_string(&args[0]);
                let mut commands = binding.split(' ').collect::<Vec<&str>>();
                let output = std::process::Command::new(commands.pop().unwrap())
                    .args(commands)
                    .spawn();
                match output {
                    Ok(_) => Value::None,
                    Err(err) => Value::Error(err.to_string()),
                }
            }
        })),
    ));
    natives.push((
        String::from("is_nullptr"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("is_nullptr requires exactly one argument"))
            } else {
                match &args[0] {
                    Value::PtrWrapper(ptr) => {
                        if ptr.is_null() {
                            Value::Boolean(true)
                        } else {
                            Value::Boolean(false)
                        }
                    }
                    _ => return Value::Boolean(false),
                }
            }
        })),
    ));
    natives.push((
        String::from("readptr"),
        Function::Native(Arc::new(|args| {
            if args.len() < 1 {
                Value::Error(String::from("readptr requires exactly one argument"))
            } else {
                match &args[0] {
                    Value::PtrWrapper(ptr) => {
                        if ptr.is_null() {
                            return Value::Error(String::from("Tried to read from nullptr"));
                        } else {
                            unsafe {
                                let result = std::ptr::read(*ptr as *const Value);
                                return result;
                            }
                        }
                    }
                    _ => return Value::Error(String::from("Cannot read from a non-ptr type")),
                }
            }
        })),
    ));
    natives.push((
        String::from("writeptr"),
        Function::Native(Arc::new(|args| {
            if args.len() < 2 {
                return Value::Error(String::from("writeptr requires exactly two arguments"));
            } else {
                match &args[0] {
                    Value::PtrWrapper(ptr) => {
                        if ptr.is_null() {
                            return Value::Error(String::from("Tried to write to a nullptr"));
                        } else {
                            unsafe { std::ptr::write(*ptr, args[1].clone()) };
                            return Value::None;
                        }
                    }
                    _ => return Value::Error(String::from("Cannot write to a non-ptr type")),
                }
            }
        })),
    ));
    natives.push((
        String::from("copyptr"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("copyptr requires exactly one argument"))
            } else {
                match &args[0] {
                    Value::PtrWrapper(ptr) => {
                        if ptr.is_null() {
                            return Value::Error(String::from("Tried to read from nullptr"));
                        } else {
                            let mut result = Value::None;
                            unsafe {
                                std::ptr::copy_nonoverlapping(
                                    *ptr as *const Value,
                                    &mut result as *mut Value,
                                    1,
                                );
                            }
                            return result;
                        }
                    }
                    _ => return Value::Error(String::from("Cannot read from a non-ptr type")),
                }
            }
        })),
    ));
    natives.push((
        String::from("as_ptr"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("asptr requires exactly one argument"))
            } else {
                return Value::PtrWrapper(&args[0] as *const Value as *mut Value);
            }
        })),
    ));
    natives.push((
        String::from("get_nullptr"),
        Function::Native(Arc::new(|args| {
            if args.len() != 0 {
                Value::Error(String::from("get_nullptr requires exactly zero arguments"))
            } else {
                return Value::PtrWrapper(std::ptr::null_mut() as *mut Value);
            }
        })),
    ));
    natives.push((
        String::from("ftruncate"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                Value::Error(String::from("ftruncate requires exactly two arguments"))
            } else {
                if std::path::Path::new(&value_to_string(&args[0])).exists() {
                    let file = std::fs::File::open(&value_to_string(&args[0]));
                    match file {
                        Ok(file) => match &args[1] {
                            Value::Int(size) => {
                                if size < &1 {
                                    return Value::Error(String::from(
                                        "Size must be greater than 0",
                                    ));
                                }
                                let res = file.set_len(*size as u64);
                                if let Err(err) = res {
                                    return Value::Error(format!(
                                        "Could not truncate file: {}",
                                        err
                                    ));
                                }
                                return Value::None;
                            }
                            Value::BigInt(size) => {
                                if size < &1 {
                                    return Value::Error(String::from(
                                        "Size must be greater than 0",
                                    ));
                                }
                                let res = file.set_len(*size as u64);
                                if let Err(err) = res {
                                    return Value::Error(format!(
                                        "Could not truncate file: {}",
                                        err
                                    ));
                                }
                                return Value::None;
                            }
                            _ => {
                                return Value::Error(format!(
                                    "{} is not a numeric type",
                                    value_to_readable(&args[1])
                                ))
                            }
                        },
                        Err(err) => return Value::Error(format!("Could open file: {}", err)),
                    }
                } else {
                    return Value::Error(format!(
                        "File {} does not exist",
                        value_to_string(&args[1])
                    ));
                }
            }
        })),
    ));

    natives.push((
        String::from("fread"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("fread requires exactly one argument"))
            } else {
                if std::path::Path::new(&value_to_string(&args[0])).exists() {
                    let file = std::fs::File::open(&value_to_string(&args[0]));
                    match file {
                        Ok(mut file) => {
                            let mut buffer = String::new();
                            let res = file.read_to_string(&mut buffer);
                            if let Err(err) = res {
                                return Value::Error(format!("Could not read from file: {}", err));
                            }
                            return Value::String(buffer);
                        }
                        Err(err) => return Value::Error(format!("Could not open file: {}", err)),
                    }
                } else {
                    return Value::Error(format!(
                        "File {} does not exist",
                        value_to_string(&args[1])
                    ));
                }
            }
        })),
    ));

    natives.push((
        String::from("fmove"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                Value::Error(String::from("move_file requires exactly two arguments"))
            } else {
                let from = value_to_string(&args[0]);
                let to = value_to_string(&args[1]);
                match std::fs::rename(from, to) {
                    Ok(_) => Value::None,
                    Err(err) => Value::Error(format!("Could not move file: {}", err)),
                }
            }
        })),
    ));

    natives.push((
        String::from("fcopy"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                Value::Error(String::from("copy_file requires exactly two arguments"))
            } else {
                let from = value_to_string(&args[0]);
                let to = value_to_string(&args[1]);
                match std::fs::copy(from, to) {
                    Ok(_) => Value::None,
                    Err(err) => Value::Error(format!("Could not copy file: {}", err)),
                }
            }
        })),
    ));

    natives.push((
        String::from("frename"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                Value::Error(String::from("rename_file requires exactly two arguments"))
            } else {
                let from = value_to_string(&args[0]);
                let to = value_to_string(&args[1]);
                match std::fs::rename(from, to) {
                    Ok(_) => Value::None,
                    Err(err) => Value::Error(format!("Could not rename file: {}", err)),
                }
            }
        })),
    ));

    natives.push((
        String::from("fexists"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("fexists requires exactly one argument"))
            } else {
                Value::Boolean(std::path::Path::new(&value_to_string(&args[0])).exists())
            }
        })),
    ));

    natives.push((
        String::from("fis_directory"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("is_directory requires exactly one argument"))
            } else {
                if std::path::Path::new(&value_to_string(&args[0])).exists() {
                    let file = std::fs::File::open(&value_to_string(&args[0]));
                    match file {
                        Ok(file) => {
                            let metadata = file.metadata();
                            if let Ok(metadata) = metadata {
                                return Value::Boolean(metadata.is_dir());
                            } else {
                                return Value::Error(format!(
                                    "Could not get metadata from file: {}",
                                    metadata.err().unwrap()
                                ));
                            }
                        }
                        Err(err) => return Value::Error(format!("Could not open file: {}", err)),
                    }
                } else {
                    return Value::Error(format!(
                        "File {} does not exist",
                        value_to_string(&args[1])
                    ));
                }
            }
        })),
    ));

    natives.push((
        String::from("fis_symlink"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("is_symlink requires exactly one argument"))
            } else {
                if std::path::Path::new(&value_to_string(&args[0])).exists() {
                    let file = std::fs::File::open(&value_to_string(&args[0]));
                    match file {
                        Ok(file) => {
                            let metadata = file.metadata();
                            if let Ok(metadata) = metadata {
                                return Value::Boolean(metadata.is_symlink());
                            } else {
                                return Value::Error(format!(
                                    "Could not get metadata from file: {}",
                                    metadata.err().unwrap()
                                ));
                            }
                        }
                        Err(err) => return Value::Error(format!("Could not open file: {}", err)),
                    }
                } else {
                    return Value::Error(format!(
                        "File {} does not exist",
                        value_to_string(&args[1])
                    ));
                }
            }
        })),
    ));

    natives.push((
        String::from("fread_exact"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                Value::Error(String::from("fread_exact requires exactly two arguments"))
            } else {
                if std::path::Path::new(&value_to_string(&args[0])).exists() {
                    let file = std::fs::File::open(&value_to_string(&args[0]));
                    match file {
                        Ok(mut file) => match &args[2] {
                            Value::Int(size) => {
                                if size < &1 {
                                    return Value::Error(String::from(
                                        "Size must be greater than 0",
                                    ));
                                }
                                let mut buffer = Vec::with_capacity(*size as usize);
                                let res = file.read_exact(&mut buffer);
                                if let Err(err) = res {
                                    return Value::Error(format!(
                                        "Could not read from file: {}",
                                        err
                                    ));
                                }
                                return Value::String(
                                    String::from_utf8_lossy(buffer.as_slice()).to_string(),
                                );
                            }
                            Value::BigInt(size) => {
                                if size < &1 {
                                    return Value::Error(String::from(
                                        "Size must be greater than 0",
                                    ));
                                }
                                let mut buffer = Vec::with_capacity(*size as usize);
                                let res = file.read_exact(&mut buffer);
                                if let Err(err) = res {
                                    return Value::Error(format!(
                                        "Could not read from file: {}",
                                        err
                                    ));
                                }
                                return Value::String(
                                    String::from_utf8_lossy(buffer.as_slice()).to_string(),
                                );
                            }
                            _ => {
                                return Value::Error(format!(
                                    "{} is not a numeric type",
                                    value_to_readable(&args[2])
                                ))
                            }
                        },
                        Err(err) => return Value::Error(format!("Could not open file: {}", err)),
                    }
                } else {
                    return Value::Error(format!(
                        "File {} does not exist",
                        value_to_string(&args[1])
                    ));
                }
            }
        })),
    ));
    natives.push((
        String::from("fwrite"),
        Function::Native(Arc::new(|args| {
            if args.len() < 2 {
                Value::Error(String::from("fwrite requires at least two arguments"))
            } else {
                if std::path::Path::new(&value_to_string(&args[0])).exists() {
                    let file = std::fs::File::open(&value_to_string(&args[0]));
                    match file {
                        Ok(mut file) => {
                            let mut result = String::new();
                            for arg in &args[1..] {
                                result.push_str(&value_to_string(arg).as_str());
                            }
                            let res = file.write_all(result.as_bytes());
                            if let Err(err) = res {
                                return Value::Error(format!("Could not write to file: {}", err));
                            }
                            Value::None
                        }
                        Err(err) => return Value::Error(format!("Could not open file: {}", err)),
                    }
                } else {
                    return Value::Error(format!(
                        "File {} does not exist",
                        value_to_string(&args[1])
                    ));
                }
            }
        })),
    ));

    natives.push((
        String::from("exit"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("exit requires exactly one argument"));
            }

            if let Value::Int(code) = &args[0] {
                std::process::exit(*code);
            }

            Value::Error(String::from(
                "arr_get takes a pointer and an integer as parameters",
            ))
        })),
    ));

    natives.push((
        String::from("arr_get"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("arr_get requires exactly two arguments"));
            }

            if let Value::PtrWrapper(ptr) = &args[0] {
                if ptr.is_null() {
                    return Value::Error(String::from("Cannot read from a null pointer"));
                }

                if let Value::Int(index) = &args[1] {
                    if *index < 1 {
                        return Value::Error(String::from("Index must be greater than 0"));
                    }

                    let list_ptr = *ptr as *mut Value;
                    let list = unsafe { &mut *list_ptr };

                    if let Value::List(elements) = list {
                        if let Some(value) = elements.get(*index as usize - 1) {
                            return value.clone();
                        }
                    }

                    return Value::None;
                }
            }

            Value::Error(String::from(
                "arr_get takes a pointer and an integer as parameters",
            ))
        })),
    ));

    natives.push((
        String::from("arr_push"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("arr_push requires exactly two arguments"));
            }

            if let Value::PtrWrapper(ptr) = &args[0] {
                if ptr.is_null() {
                    return Value::Error(String::from("Cannot read from a null pointer"));
                }

                let list_ptr = *ptr as *mut Value;
                let list = unsafe { &mut *list_ptr };

                if let Value::List(elements) = list {
                    elements.push(args[1].clone());
                    return Value::None;
                }

                return Value::Error(String::from(
                    "arr_push takes a pointer to a list as the first parameter",
                ));
            }

            Value::Error(String::from(
                "arr_push takes a pointer and a value as parameters",
            ))
        })),
    ));

    natives.push((
        String::from("contains"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("contains requires exactly two arguments"));
            }

            if let Value::PtrWrapper(ptr) = &args[0] {
                if ptr.is_null() {
                    return Value::Error(String::from("Cannot read from a null pointer"));
                }

                let list_ptr = *ptr as *mut Value;
                let list = unsafe { &mut *list_ptr };

                if let Value::List(elements) = list {
                    return Value::Boolean(elements.contains(&args[1]));
                }

                return Value::Error(String::from(
                    "contains takes a pointer to a list as the first parameter",
                ));
            }

            Value::Error(String::from(
                "contains takes a pointer and a value as parameters",
            ))
        })),
    ));

    natives.push((
        String::from("arr_insert"),
        Function::Native(Arc::new(|args| {
            if args.len() != 3 {
                return Value::Error(String::from("arr_insert requires exactly three arguments"));
            }

            if let Value::PtrWrapper(ptr) = &args[0] {
                if ptr.is_null() {
                    return Value::Error(String::from("Cannot read from a null pointer"));
                }

                let list_ptr = *ptr as *mut Value;
                let list = unsafe { &mut *list_ptr };

                if let Value::List(elements) = list {
                    if let Value::Int(index) = &args[1] {
                        if *index < 1 || *index > (elements.len() as i32 + 1) {
                            return Value::Error(format!("Invalid index: {}", *index));
                        }

                        elements.insert(*index as usize - 1, args[2].clone());
                        return Value::None;
                    }
                }

                return Value::Error(String::from(
                    "arr_insert takes a pointer, an integer index, and a value as parameters",
                ));
            }

            Value::Error(String::from(
                "arr_insert takes a pointer, an integer index, and a value as parameters",
            ))
        })),
    ));

    natives.push((
        String::from("arr_len"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("arr_len requires exactly one argument"));
            }

            if let Value::PtrWrapper(ptr) = &args[0] {
                if ptr.is_null() {
                    return Value::Error(String::from("Cannot read from a null pointer"));
                }

                let list_ptr = *ptr as *mut Value;
                let list = unsafe { &mut *list_ptr };

                if let Value::List(elements) = list {
                    return Value::Int(elements.len() as i32);
                }

                return Value::Error(String::from(
                    "arr_len takes a pointer to a list as the parameter",
                ));
            }

            Value::Error(String::from(
                "arr_len takes a pointer to a list as the parameter",
            ))
        })),
    ));

    natives.push((
        String::from("arr_indexof"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("arr_indexof requires exactly two arguments"));
            }

            if let Value::PtrWrapper(ptr) = &args[0] {
                if ptr.is_null() {
                    return Value::Error(String::from("Cannot read from a null pointer"));
                }

                let list_ptr = *ptr as *mut Value;
                let list = unsafe { &mut *list_ptr };

                if let Value::List(elements) = list {
                    let value_to_find = &args[1];
                    if let Some(index) = elements.iter().position(|v| v == value_to_find) {
                        return Value::Int((index as i32) + 1);
                    } else {
                        return Value::None;
                    }
                }

                return Value::Error(String::from(
                    "arr_indexof takes a pointer to a list and a value as parameters",
                ));
            }

            Value::Error(String::from(
                "arr_indexof takes a pointer to a list and a value as parameters",
            ))
        })),
    ));

    natives.push((
        String::from("arr_first"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("arr_first requires exactly one argument"));
            }

            if let Value::PtrWrapper(ptr) = &args[0] {
                if ptr.is_null() {
                    return Value::Error(String::from("Cannot read from a null pointer"));
                }

                let list_ptr = *ptr as *mut Value;
                let list = unsafe { &mut *list_ptr };

                if let Value::List(elements) = list {
                    if let Some(first) = elements.first() {
                        return Value::PtrWrapper(first as *const Value as *mut Value);
                    } else {
                        return Value::None;
                    }
                }

                return Value::Error(String::from(
                    "arr_first takes a pointer to a list as the parameter",
                ));
            }

            Value::Error(String::from(
                "arr_first takes a pointer to a list as the parameter",
            ))
        })),
    ));

    natives.push((
        String::from("arr_last"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("arr_last requires exactly one argument"));
            }

            if let Value::PtrWrapper(ptr) = &args[0] {
                if ptr.is_null() {
                    return Value::Error(String::from("Cannot read from a null pointer"));
                }

                let list_ptr = *ptr as *mut Value;
                let list = unsafe { &mut *list_ptr };

                if let Value::List(elements) = list {
                    if let Some(last) = elements.last() {
                        return Value::PtrWrapper(last as *const Value as *mut Value);
                    } else {
                        return Value::None;
                    }
                }

                return Value::Error(String::from(
                    "arr_last takes a pointer to a list as the parameter",
                ));
            }

            Value::Error(String::from(
                "arr_last takes a pointer to a list as the parameter",
            ))
        })),
    ));

    natives.push((
        String::from("reverse"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("reverse requires exactly one argument"));
            }

            if let Value::PtrWrapper(ptr) = &args[0] {
                if ptr.is_null() {
                    return Value::Error(String::from("Cannot read from a null pointer"));
                }

                let list_ptr = *ptr as *mut Value;
                let list = unsafe { &mut *list_ptr };

                if let Value::List(elements) = list {
                    let mut elemscopy = elements.clone();
                    elemscopy.reverse();
                    return Value::List(elemscopy);
                }

                return Value::Error(String::from(
                    "arr_last takes a pointer to a list as the parameter",
                ));
            } else if let Value::String(string) = &args[0] {
                let mut list_of_chars = string.chars().collect::<Vec<char>>();
                list_of_chars.reverse();
                let mut new_string = String::new();
                for character in list_of_chars {
                    new_string.push(character);
                }
                return Value::String(new_string);
            }

            Value::Error(String::from(
                "arr_last takes a pointer to a list or a string as the parameter",
            ))
        })),
    ));

    natives.push((
        String::from("zip"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("zip requires exactly one argument"));
            }

            if let Value::PtrWrapper(ptr) = &args[0] {
                if ptr.is_null() {
                    return Value::Error(String::from("Cannot read from a null pointer"));
                }

                let list_ptr = *ptr as *mut Value;
                let list = unsafe { &mut *list_ptr };

                if let Value::List(elements) = list {
                    if let Value::PtrWrapper(ptr2) = &args[1] {
                        if ptr2.is_null() {
                            return Value::Error(String::from("Cannot read from a null pointer"));
                        }

                        let list_ptr2 = *ptr2 as *mut Value;
                        let list2 = unsafe { &mut *list_ptr2 };

                        if let Value::List(elements2) = list2 {
                            let mut new_list: Vec<Value> = vec![];
                            if elements.len() != elements2.len() {
                                return Value::Error(String::from(
                                    "To use zip, the length of the lists must be equal",
                                ));
                            }
                            for (element1, element2) in elements
                                .into_iter()
                                .zip(elements2.into_iter())
                                .collect::<Vec<(&mut Value, &mut Value)>>()
                            {
                                new_list
                                    .push(Value::Tuple(vec![element1.clone(), element2.clone()]));
                            }
                            return Value::List(new_list);
                        }

                        return Value::Error(String::from(
                            "zip takes a pointer to a list as the second parameter",
                        ));
                    }
                }

                return Value::Error(String::from(
                    "zip takes a pointer to a list as the first parameter",
                ));
            }

            Value::Error(String::from(
                "zip takes a pointer to a list as the first parameter",
            ))
        })),
    ));

    natives.push((
        String::from("enumerate"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("enumerate requires exactly one argument"));
            }

            if let Value::PtrWrapper(ptr) = &args[0] {
                if ptr.is_null() {
                    return Value::Error(String::from("Cannot read from a null pointer"));
                }

                let list_ptr = *ptr as *mut Value;
                let list = unsafe { &mut *list_ptr };

                if let Value::List(elements) = list {
                    let mut new_list: Vec<Value> = vec![];
                    let mut counter = 0;
                    for element in elements {
                        new_list.push(Value::Tuple(vec![Value::Int(counter), element.clone()]));
                        counter += 1;
                    }
                    return Value::List(new_list);
                }

                return Value::Error(String::from(
                    "enumerate takes a pointer to a list as the first parameter",
                ));
            }

            Value::Error(String::from(
                "enumerate takes a pointer to a list as the first parameter",
            ))
        })),
    ));

    natives.push((
        String::from("max"),
        Function::Native(Arc::new(|args| {
            if args.len() < 2 {
                Value::Error(String::from("max requires at least two arguments"))
            } else {
                let mut max: f64 = 0.0;
                for arg in args {
                    match arg {
                        Value::BigInt(i) => {
                            if *i as f64 > max {
                                max = *i as f64;
                            }
                        }
                        Value::Int(i) => {
                            if *i as f64 > max {
                                max = *i as f64;
                            }
                        }
                        Value::Float(f) => {
                            if *f > max {
                                max = *f;
                            }
                        }
                        Value::LFloat(f) => {
                            if *f as f64 > max {
                                max = *f as f64;
                            }
                        }
                        _ => {
                            return Value::Error(format!(
                                "{} is not a numeric type",
                                value_to_readable(arg)
                            ))
                        }
                    }
                }
                Value::Float(max)
            }
        })),
    ));

    natives.push((
        String::from("min"),
        Function::Native(Arc::new(|args| {
            if args.len() < 2 {
                Value::Error(String::from("min requires at least two arguments"))
            } else {
                let mut min: f64 = 0.0;
                for arg in args {
                    match arg {
                        Value::BigInt(i) => {
                            if (*i as f64) < min {
                                min = *i as f64;
                            }
                        }
                        Value::Int(i) => {
                            if (*i as f64) < min {
                                min = *i as f64;
                            }
                        }
                        Value::Float(f) => {
                            if *f < min {
                                min = *f;
                            }
                        }
                        Value::LFloat(f) => {
                            if (*f as f64) < min {
                                min = *f as f64;
                            }
                        }
                        _ => {
                            return Value::Error(format!(
                                "{} is not a numeric type",
                                value_to_readable(arg)
                            ))
                        }
                    }
                }
                Value::Float(min)
            }
        })),
    ));

    natives.push((
        String::from("t_getelem"), // tuple get element
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("t_getelem requires exactly two arguments"));
            }

            if let Value::PtrWrapper(ptr) = &args[0] {
                if ptr.is_null() {
                    return Value::Error(String::from("Cannot read from a null pointer"));
                }

                if let Value::Int(index) = &args[1] {
                    if *index < 1 {
                        return Value::Error(String::from("Index must be greater than 0"));
                    }

                    let list_ptr = *ptr as *mut Value;
                    let list = unsafe { &mut *list_ptr };

                    if let Value::Tuple(elements) = list {
                        if let Some(value) = elements.get(*index as usize - 1) {
                            return value.clone();
                        }
                    }

                    return Value::None;
                }
            }

            Value::Error(String::from(
                "t_getelem takes a pointer and an integer as parameters",
            ))
        })),
    ));

    natives.push((
        String::from("http_get"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("http_get requires exactly one argument"))
            } else {
                let url = value_to_string(&args[0]);
                let body = reqwest::blocking::get(&url)
                    .and_then(|res| res.text())
                    .unwrap_or_else(|err| format!("Error: {}", err));
                Value::String(body)
            }
        })),
    ));

    natives.push((
        String::from("http_post"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                Value::Error(String::from("http_post requires exactly two arguments"))
            } else {
                let url = value_to_string(&args[0]);
                let data = value_to_string(&args[1]);
                let client = reqwest::blocking::Client::new();
                let body = client
                    .post(&url)
                    .body(data)
                    .send()
                    .and_then(|res| res.text())
                    .unwrap_or_else(|err| format!("Error: {}", err));
                Value::String(body)
            }
        })),
    ));

    natives.push((
        String::from("http_put"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                Value::Error(String::from("http_put requires exactly two arguments"))
            } else {
                let url = value_to_string(&args[0]);
                let data = value_to_string(&args[1]);
                let client = reqwest::blocking::Client::new();
                let body = client
                    .put(&url)
                    .body(data)
                    .send()
                    .and_then(|res| res.text())
                    .unwrap_or_else(|err| format!("Error: {}", err));
                Value::String(body)
            }
        })),
    ));

    natives.push((
        String::from("http_delete"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("http_delete requires exactly one argument"))
            } else {
                let url = value_to_string(&args[0]);
                let client = reqwest::blocking::Client::new();
                let body = client
                    .delete(&url)
                    .send()
                    .and_then(|res| res.text())
                    .unwrap_or_else(|err| format!("Error: {}", err));
                Value::String(body)
            }
        })),
    ));

    let mathematical = get_math();

    natives.extend(mathematical);

    natives
}

pub fn get_math() -> Vec<(String, Function)> {
    let mut natives = vec![];
    natives.push((
        String::from("abs"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("abs requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.abs()),
                Value::LFloat(f) => Value::LFloat(f.abs()),
                Value::Int(i) => Value::Int(i.abs()),
                Value::BigInt(bi) => Value::BigInt(bi.abs()),
                _ => Value::Error(String::from("abs requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("sin"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("sin requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.sin()),
                Value::LFloat(f) => Value::LFloat(f.sin()),
                _ => Value::Error(String::from("sin requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("cos"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("cos requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.cos()),
                Value::LFloat(f) => Value::LFloat(f.cos()),
                _ => Value::Error(String::from("cos requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("tan"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("tan requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.tan()),
                Value::LFloat(f) => Value::LFloat(f.tan()),
                _ => Value::Error(String::from("tan requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("asin"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("asin requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.asin()),
                Value::LFloat(f) => Value::LFloat(f.asin()),
                _ => Value::Error(String::from("asin requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("acos"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("acos requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.acos()),
                Value::LFloat(f) => Value::LFloat(f.acos()),
                _ => Value::Error(String::from("acos requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("atan"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("atan requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.atan()),
                Value::LFloat(f) => Value::LFloat(f.atan()),
                _ => Value::Error(String::from("atan requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("sinh"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("sinh requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.sinh()),
                Value::LFloat(f) => Value::LFloat(f.sinh()),
                _ => Value::Error(String::from("sinh requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("cosh"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("cosh requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.cosh()),
                Value::LFloat(f) => Value::LFloat(f.cosh()),
                _ => Value::Error(String::from("cosh requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("tanh"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("tanh requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.tanh()),
                Value::LFloat(f) => Value::LFloat(f.tanh()),
                _ => Value::Error(String::from("tanh requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("asinh"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("asinh requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.asinh()),
                Value::LFloat(f) => Value::LFloat(f.asinh()),
                _ => Value::Error(String::from("asinh requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("acosh"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("acosh requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.acosh()),
                Value::LFloat(f) => Value::LFloat(f.acosh()),
                _ => Value::Error(String::from("acosh requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("atanh"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("atanh requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.atanh()),
                Value::LFloat(f) => Value::LFloat(f.atanh()),
                _ => Value::Error(String::from("atanh requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("atan2"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("atan2 requires exactly one argument"));
            }

            match (&args[0], &args[1]) {
                (Value::Float(f), Value::Float(f2)) => Value::Float(f.atan2(*f2)),
                (Value::LFloat(f), Value::LFloat(f2)) => Value::LFloat(f.atan2(*f2)),
                _ => Value::Error(String::from("atan2 requires comparible numeric arguments")),
            }
        })),
    ));

    natives.push((
        String::from("exp"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("exp requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.exp()),
                Value::LFloat(f) => Value::LFloat(f.exp()),
                _ => Value::Error(String::from("exp requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("log"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("log requires exactly two arguments"));
            }

            match (&args[0], &args[1]) {
                (Value::Float(x), Value::Float(base)) => Value::Float(x.log(*base)),
                (Value::LFloat(x), Value::LFloat(base)) => Value::LFloat(x.log(*base)),
                _ => Value::Error(String::from("log requires numeric arguments")),
            }
        })),
    ));

    natives.push((
        String::from("sqrt"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("sqrt requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.sqrt()),
                Value::LFloat(f) => Value::LFloat(f.sqrt()),
                _ => Value::Error(String::from("sqrt requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("floor"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("floor requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.floor()),
                Value::LFloat(f) => Value::LFloat(f.floor()),
                _ => Value::Error(String::from("floor requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("platform"),
        Function::Native(Arc::new(|args| {
            if args.len() != 0 {
                return Value::Error(String::from("cplatform requires exactly zero arguments"));
            }

            return Value::String(std::env::consts::OS.to_string());
        })),
    ));

    natives.push((
        String::from("ceil"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("ceil requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.ceil()),
                Value::LFloat(f) => Value::LFloat(f.ceil()),
                _ => Value::Error(String::from("ceil requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("round"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("round requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.round()),
                Value::LFloat(f) => Value::LFloat(f.round()),
                _ => Value::Error(String::from("round requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("trunc"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("trunc requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.trunc()),
                Value::LFloat(f) => Value::LFloat(f.trunc()),
                _ => Value::Error(String::from("trunc requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("pow"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("pow requires exactly two arguments"));
            }

            match (&args[0], &args[1]) {
                (Value::Float(x), Value::Float(y)) => Value::Float(x.powf(*y)),
                (Value::LFloat(x), Value::LFloat(y)) => Value::LFloat(x.powf(*y)),
                _ => Value::Error(String::from("pow requires numeric arguments")),
            }
        })),
    ));

    natives.push((
        String::from("exp"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("exp requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.exp()),
                Value::LFloat(f) => Value::LFloat(f.exp()),
                _ => Value::Error(String::from("exp requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("ln"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("ln requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => {
                    if *f > 0.0 {
                        Value::Float(f.ln())
                    } else {
                        Value::Error(String::from("ln requires a positive numeric argument"))
                    }
                }
                Value::LFloat(f) => {
                    if *f > 0.0 {
                        Value::LFloat(f.ln())
                    } else {
                        Value::Error(String::from("ln requires a positive numeric argument"))
                    }
                }
                _ => Value::Error(String::from("ln requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("log10"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("log10 requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => {
                    if *f > 0.0 {
                        Value::Float(f.log10())
                    } else {
                        Value::Error(String::from("log10 requires a positive numeric argument"))
                    }
                }
                Value::LFloat(f) => {
                    if *f > 0.0 {
                        Value::LFloat(f.log10())
                    } else {
                        Value::Error(String::from("log10 requires a positive numeric argument"))
                    }
                }
                _ => Value::Error(String::from("log10 requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("log2"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("log2 requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => {
                    if *f > 0.0 {
                        Value::Float(f.log10())
                    } else {
                        Value::Error(String::from("log2 requires a positive numeric argument"))
                    }
                }
                Value::LFloat(f) => {
                    if *f > 0.0 {
                        Value::LFloat(f.log2())
                    } else {
                        Value::Error(String::from("log2 requires a positive numeric argument"))
                    }
                }
                _ => Value::Error(String::from("log2 requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("pow"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("pow requires exactly two arguments"));
            }

            match (&args[0], &args[1]) {
                (Value::Float(base), Value::Float(exponent)) => Value::Float(base.powf(*exponent)),
                (Value::Float(base), Value::LFloat(exponent)) => {
                    Value::Float(base.powf(*exponent as f64))
                }
                (Value::LFloat(base), Value::Float(exponent)) => {
                    Value::Float((*base as f64).powf(*exponent))
                }
                (Value::LFloat(base), Value::LFloat(exponent)) => {
                    Value::Float((*base as f64).powf(*exponent as f64))
                }
                _ => Value::Error(String::from("pow requires numeric arguments")),
            }
        })),
    ));

    natives.push((
        String::from("round"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("round requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.round()),
                Value::LFloat(f) => Value::LFloat(f.round()),
                _ => Value::Error(String::from("round requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("sleep"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("sleep requires exactly one argument"));
            }

            match &args[0] {
                Value::Int(n) => {
                    if n <= &0 {
                        return Value::Error(String::from("sleep requires a value bigger than zero"));
                    }
                    std::thread::sleep(Duration::from_secs(*n as u64));
                    return Value::None;
                }
                Value::BigInt(n) => {
                    if n <= &0 {
                        return Value::Error(String::from("sleep requires a value bigger than zero"));
                    }
                    std::thread::sleep(Duration::from_secs(*n as u64));
                    return Value::None;
                }
                _ => Value::Error(String::from("sleep requires a numeric argument")),
            }
        }))
    ));

    natives.push((
        String::from("panic"),
        Function::Native(Arc::new(|args| {
            if args.len() != 0 {
                let mut result = String::new();
                for val in args {
                    result.push_str(&value_to_string(val));
                }
                eprintln!("{}: program PANICKED at `{}` with message: {}", Color::Red.bold().paint("RuntimePanic"), get_current_time(), result);
                std::process::exit(1);
            } else {
                eprintln!("{}: program PANICKED at `{}` without a message", Color::Red.bold().paint("RuntimePanic"), get_current_time());
                std::process::exit(1);
            }
        }))
    ));

    natives.push((
        String::from("get_time"),
        Function::Native(Arc::new(|_| {
            let local: DateTime<Local> = Local::now();
            Value::String(local.format("%H:%M:%S").to_string())
        }))
    ));

    natives.push((
        String::from("getchar"),
        Function::Native(Arc::new(|_| {
            let mut buffer = [0; 1];
            let lock = stdin().read(&mut buffer);
            if let Err(err) = lock {
                return Value::Error(format!("Failed to read from stdin: {}", err));
            }
            return Value::Character(buffer[0] as char);
        }))
    ));

    natives
}

pub fn get_half_natives() -> Vec<(String, Function)> {
    let mut natives: Vec<(String, Function)> = Vec::new();
    
    natives.push(("open_file".to_string(), Function::Interpreted(FunctionStruct {
        name: "open_file".to_string(),
        args: vec![("file_name".to_string(), Types::String)],
        returns: Types::FileHandle,
        body: [
            Instruction::SequestrateVariables,
            Instruction::Push(Value::String("open_file".to_string())),
            Instruction::PopToRoot(String::from("__function__")),
            Instruction::AllocArgsToLocal,
            Instruction::Load("file_name".to_string()),
            Instruction::GetReadFileHandleStack,
            Instruction::Return,
        ].to_vec(),
    })));

    natives.push(("create_file".to_string(), Function::Interpreted(FunctionStruct {
        name: "create_file".to_string(),
        args: vec![("file_name".to_string(), Types::String)],
        returns: Types::FileHandle,
        body: [
            Instruction::SequestrateVariables,
            Instruction::Push(Value::String("create_file".to_string())),
            Instruction::PopToRoot(String::from("__function__")),
            Instruction::AllocArgsToLocal,
            Instruction::Load("file_name".to_string()),
            Instruction::GetWriteFileHandleStack,
            Instruction::Return,
        ].to_vec(),
    })));

    return natives;
}