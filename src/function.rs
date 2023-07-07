use std::fmt::Debug;
use std::io::stdin;
use std::io::{Read, Write};
use std::ptr::NonNull;
use std::sync::Arc;

use crate::vm::{value_to_readable, value_to_string, Instruction, Types, Value};

use std::ops::{Mul, Sub, Add};

fn factorial<T>(n: T) -> T
where
    T: std::ops::Mul<Output = T>
        + std::ops::Sub<Output = T>
        + std::ops::Add<Output = T>
        + From<u16>
        + Clone
        + std::cmp::PartialOrd,
{
    if n.clone() <= T::from(1u16) {
        T::from(1u16)
    } else {
        n.clone() * factorial(n.clone() - T::from(1u16))
    }
}

#[derive(Clone)]
pub enum Function {
    Native(Arc<dyn Fn(&[Value]) -> Value>),
    Interpreted(FunctionStruct),
}

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
                Function::Native(_) => false,
                Function::Interpreted(s2) => s == s2,
            },
        }
    }
}

impl Debug for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Function::Native(_) => write!(f, "<builtin_function>"),
            Function::Interpreted(s) => write!(f, "Interpreted({:?})", s),
        }
    }
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
        String::from("fmt.print"),
        Function::Native(Arc::new(|args| {
            if args.is_empty() {
                Value::Error(String::from("fmt.print requires at least one argument"))
            } else {
                for arg in args {
                    print!("{}", value_to_string(arg));
                }
                Value::None
            }
        })),
    ));
    natives.push((
        String::from("fmt.println"),
        Function::Native(Arc::new(|args| {
            if args.is_empty() {
                Value::Error(String::from("fmt.println requires at least one argument"))
            } else {
                for arg in args {
                    println!("{}", value_to_string(arg));
                }
                Value::None
            }
        })),
    ));
    natives.push((
        String::from("io.read_line"),
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
        String::from("list.new"),
        Function::Native(Arc::new(|_| {
            Value::List(vec![])
        })),
    ));
    natives.push((
        String::from("concatenate"),
        Function::Native(Arc::new(|args| {
            if args.is_empty() {
                Value::Error(String::from("concatenate requires at least one argument"))
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
        String::from("string.to_upper"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("string.to_upper requires exactly one argument"))
            } else {
                Value::String(value_to_string(&args[0]).to_uppercase())
            }
        })),
    ));
    natives.push((
        String::from("string.to_lower"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("string.to_lower requires exactly one argument"))
            } else {
                Value::String(value_to_string(&args[0]).to_lowercase())
            }
        })),
    ));
    natives.push((
        String::from("string.is_numeric"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("string.is_numeric requires exactly one argument"))
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
        String::from("string.is_upper"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("string.is_upper requires exactly one argument"))
            } else {
                return Value::Boolean(value_to_string(&args[0]).chars().all(|c| c.is_uppercase()));
            }
        })),
    ));
    natives.push((
        String::from("string.is_lower"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("string.is_lower requires exactly one argument"))
            } else {
                return Value::Boolean(value_to_string(&args[0]).chars().all(|c| c.is_lowercase()));
            }
        })),
    ));
    natives.push((
        String::from("string.is_empty"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("string.is_empty requires exactly one argument"))
            } else {
                return Value::Boolean(value_to_string(&args[0]).is_empty());
            }
        })),
    ));
    natives.push((
        String::from("string.trim"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("string.is_empty requires exactly one argument"))
            } else {
                return Value::String(value_to_string(&args[0]).trim().to_string());
            }
        })),
    ));
    natives.push((
        String::from("string.is_hex"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("string.is_hex requires exactly one argument"))
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
        String::from("string.is_octal"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("string.is_octal requires exactly one argument"))
            } else {
                return Value::Boolean(value_to_string(&args[0]).chars().all(|c| c.is_digit(8)));
            }
        })),
    ));
    natives.push((
        String::from("string.is_binary"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("string.is_binary requires exactly one argument"))
            } else {
                return Value::Boolean(value_to_string(&args[0]).chars().all(|c| c.is_digit(2)));
            }
        })),
    ));
    natives.push((
        String::from("string.length"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("string.length requires exactly one argument"))
            } else {
                Value::Int(value_to_string(&args[0]).len() as i32)
            }
        })),
    ));
    // count: Counts the number of occurrences of a substring within a string.
    natives.push((
        String::from("string.count"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("string.count requires exactly two arguments"));
            }
            match (&args[0], &args[1]) {
                (Value::String(s), Value::String(sub)) => {
                    let count = s.matches(sub).count();
                    Value::Int(count as i32)
                }
                _ => Value::Error(String::from("string.count requires two string arguments")),
            }
        })),
    ));

    // starts_with: Checks if a string starts with a specific prefix.
    natives.push((
        String::from("string.starts_with"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("string.starts_with requires exactly two arguments"));
            }
            match (&args[0], &args[1]) {
                (Value::String(s), Value::String(prefix)) => {
                    let starts_with = s.starts_with(prefix);
                    Value::Boolean(starts_with)
                }
                _ => Value::Error(String::from("string.starts_with requires two string arguments")),
            }
        })),
    ));

    // ends_with: Checks if a string ends with a specific suffix.
    natives.push((
        String::from("string.ends_with"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("string.ends_with requires exactly two arguments"));
            }
            match (&args[0], &args[1]) {
                (Value::String(s), Value::String(suffix)) => {
                    let ends_with = s.ends_with(suffix);
                    Value::Boolean(ends_with)
                }
                _ => Value::Error(String::from("string.ends_with requires two string arguments")),
            }
        })),
    ));

    // replace: Replaces all occurrences of a substring within a string with another substring.
    natives.push((
        String::from("string.replace"),
        Function::Native(Arc::new(|args| {
            if args.len() != 3 {
                return Value::Error(String::from("string.replace requires exactly three arguments"));
            }
            match (&args[0], &args[1], &args[2]) {
                (Value::String(s), Value::String(old), Value::String(new)) => {
                    let replaced = s.replace(old, new);
                    Value::String(replaced)
                }
                _ => Value::Error(String::from("string.replace requires three string arguments")),
            }
        })),
    ));

    // substring: Retrieves a substring from a given string based on start and end indices.
    natives.push((
        String::from("string.substring"),
        Function::Native(Arc::new(|args| {
            if args.len() != 3 {
                return Value::Error(String::from("string.substring requires exactly three arguments"));
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
                    "string.substring requires a string and two integer arguments",
                )),
            }
        })),
    ));

    // join: Joins elements of a list into a single string using a delimiter.
    natives.push((
        String::from("list.join"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("list.join requires exactly two arguments"));
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
                _ => Value::Error(String::from("list.join requires a list and a string argument")),
            }
        })),
    ));

    // split: Splits a string into a list of substrings based on a delimiter.
    natives.push((
        String::from("string.split"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("string.split requires exactly two arguments"));
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
                    "string.split requires a string and a string delimiter",
                )),
            }
        })),
    ));

    natives.push((
        String::from("string.lines"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("string.lines requires exactly one argument"));
            }
            match &args[0] {
                Value::String(s) => {
                    let substrings: Vec<Value> = s
                        .split('\n')
                        .map(|substring| Value::String(String::from(substring)))
                        .collect();
                    Value::List(substrings)
                }
                _ => Value::Error(String::from("string.lines requires a string")),
            }
        })),
    ));

    natives.push((
        String::from("string.from"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("string.from requires exactly one argument"));
            }
            Value::String(value_to_string(&args[0]))
        })),
    ));

    natives.push((
        String::from("pointer.read"),
        Function::Native(Arc::new(|args| {
            if args.len() < 1 {
                Value::Error(String::from("pointer.read requires exactly one argument"))
            } else {
                match &args[0] {
                    Value::PtrWrapper(ptr) => {
                        unsafe {
                            let result = std::ptr::read(ptr.as_ptr() as *const Value);
                            return result;
                        }
                    }
                    _ => return Value::Error(String::from("Cannot read from a non-ptr type")),
                }
            }
        })),
    ));
    natives.push((
        String::from("pointer.write"),
        Function::Native(Arc::new(|args| {
            if args.len() < 2 {
                return Value::Error(String::from("pointer.write requires exactly two arguments"))
            } else {
                match &args[0] {
                    Value::PtrWrapper(ptr) => {
                        unsafe { std::ptr::write(ptr.as_ptr(), args[1].clone()) };
                            return Value::None;
                    }
                    _ => return Value::Error(String::from("Cannot write to a non-ptr type")),
                }
            }
        })),
    ));
    natives.push((
        String::from("pointer.copy"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("pointer.copy requires exactly one argument"))
            } else {
                match &args[0] {
                    Value::PtrWrapper(ptr) => {
                        let mut result = Value::None;
                            unsafe {
                            std::ptr::copy_nonoverlapping(
                                ptr.as_ptr() as *const Value,
                                &mut result as *mut Value,
                                1,
                            );
                        }
                        result
                    }
                    _ => return Value::Error(String::from("Cannot read from a non-ptr type")),
                }
            }
        })),
    ));
    natives.push((
        String::from("pointer.from"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("pointer.from requires exactly one argument"))
            } else {
                return Value::PtrWrapper(NonNull::new(&args[0] as *const Value as *mut Value).unwrap());
            }
        })),
    ));

    natives.push((
        String::from("process.exit"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("process.exit requires exactly one argument"));
            }

            if let Value::Int(code) = &args[0] {
                std::process::exit(*code);
            }

            Value::Error(String::from(
                "process.exit takes an integer as parameter",
            ))
        })),
    ));

    natives.push((
        String::from("list.get"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("list.get requires exactly two arguments"));
            }

            if let Value::PtrWrapper(ptr) = &args[0] {

                if let Value::Int(index) = &args[1] {
                    if *index < 1 {
                        return Value::Error(String::from("Index must be greater than 0"));
                    }

                    let list_ptr = ptr.as_ptr() as *mut Value;
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
        String::from("list.push"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("list.push requires exactly two arguments"));
            }

            if let Value::PtrWrapper(ptr) = &args[0] {

                let list_ptr = ptr.as_ptr() as *mut Value;
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
        String::from("list.contains"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("list.contains requires exactly two arguments"));
            }

            if let Value::PtrWrapper(ptr) = &args[0] {

                let list_ptr = ptr.as_ptr() as *mut Value;
                let list = unsafe { &mut *list_ptr };

                if let Value::List(elements) = list {
                    return Value::Boolean(elements.contains(&args[1]));
                }

                return Value::Error(String::from(
                    "list.contains takes a pointer to a list as the first parameter",
                ));
            }

            Value::Error(String::from(
                "list.contains takes a pointer and a value as parameters",
            ))
        })),
    ));

    natives.push((
        String::from("list.insert"),
        Function::Native(Arc::new(|args| {
            if args.len() != 3 {
                return Value::Error(String::from("list.insert requires exactly three arguments"));
            }

            if let Value::PtrWrapper(ptr) = &args[0] {

                let list_ptr = ptr.as_ptr() as *mut Value;
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
                    "list.insert takes a pointer, an integer index, and a value as parameters",
                ));
            }

            Value::Error(String::from(
                "list.insert takes a pointer, an integer index, and a value as parameters",
            ))
        })),
    ));

    natives.push((
        String::from("list.length"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("list.length requires exactly one argument"));
            }

            if let Value::PtrWrapper(ptr) = &args[0] {

                let list_ptr = ptr.as_ptr() as *mut Value;
                let list = unsafe { &mut *list_ptr };

                if let Value::List(elements) = list {
                    return Value::Int(elements.len() as i32);
                }

                return Value::Error(String::from(
                    "list.length takes a pointer to a list as the parameter",
                ));
            }

            Value::Error(String::from(
                "list.length takes a pointer to a list as the parameter",
            ))
        })),
    ));

    natives.push((
        String::from("list.find"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("list.find requires exactly two arguments"));
            }

            if let Value::PtrWrapper(ptr) = &args[0] {

                let list_ptr = ptr.as_ptr() as *mut Value;
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
                    "list.find takes a pointer to a list and a value as parameters",
                ));
            }

            Value::Error(String::from(
                "list.find takes a pointer to a list and a value as parameters",
            ))
        })),
    ));

    natives.push((
        String::from("list.first"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("list.first requires exactly one argument"));
            }

            if let Value::PtrWrapper(ptr) = &args[0] {

                let list_ptr = ptr.as_ptr() as *mut Value;
                let list = unsafe { &mut *list_ptr };

                if let Value::List(elements) = list {
                    if let Some(first) = elements.first() {
                        return Value::PtrWrapper(NonNull::new(first as *const Value as *mut Value).unwrap());
                    } else {
                        return Value::None;
                    }
                }

                return Value::Error(String::from(
                    "list.first takes a pointer to a list as the parameter",
                ));
            }

            Value::Error(String::from(
                "list.first takes a pointer to a list as the parameter",
            ))
        })),
    ));

    natives.push((
        String::from("list.last"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("list.last requires exactly one argument"));
            }

            if let Value::PtrWrapper(ptr) = &args[0] {
                let list_ptr = ptr.as_ptr() as *mut Value;
                let list = unsafe { &mut *list_ptr };

                if let Value::List(elements) = list {
                    if let Some(last) = elements.last() {
                        return Value::PtrWrapper(NonNull::new(last as *const Value as *mut Value).unwrap());
                    } else {
                        return Value::None;
                    }
                }

                return Value::Error(String::from(
                    "list.last takes a pointer to a list as the parameter",
                ));
            }

            Value::Error(String::from(
                "list.last takes a pointer to a list as the parameter",
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

                let list_ptr = ptr.as_ptr() as *mut Value;
                let list = unsafe { &mut *list_ptr };

                if let Value::List(elements) = list {
                    let mut elemscopy = elements.clone();
                    elemscopy.reverse();
                    return Value::List(elemscopy);
                }

                return Value::Error(String::from(
                    "reverse takes a pointer to a list as the parameter",
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
                "reverse takes a pointer to a list or a string as the parameter",
            ))
        })),
    ));

    natives.push((
        String::from("list.zip"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("list.zip requires exactly one argument"));
            }

            if let Value::PtrWrapper(ptr) = &args[0] {

                let list_ptr = ptr.as_ptr() as *mut Value;
                let list = unsafe { &mut *list_ptr };

                if let Value::List(elements) = list {
                    if let Value::PtrWrapper(ptr2) = &args[1] {

                        let list_ptr2 = ptr2.as_ptr() as *mut Value;
                        let list2 = unsafe { &mut *list_ptr2 };

                        if let Value::List(elements2) = list2 {
                            let mut new_list: Vec<Value> = vec![];
                            if elements.len() != elements2.len() {
                                return Value::Error(String::from(
                                    "To use list.zip, the length of the lists must be equal",
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
                            "list.zip takes a pointer to a list as the second parameter",
                        ));
                    }
                }

                return Value::Error(String::from(
                    "list.zip takes a pointer to a list as the first parameter",
                ));
            }

            Value::Error(String::from(
                "zip takes a pointer to a list as the first parameter",
            ))
        })),
    ));

    natives.push((
        String::from("list.enumerate"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("list.enumerate requires exactly one argument"));
            }

            if let Value::PtrWrapper(ptr) = &args[0] {

                let list_ptr = ptr.as_ptr() as *mut Value;
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
                    "list.enumerate takes a pointer to a list as the first parameter",
                ));
            }

            Value::Error(String::from(
                "list.enumerate takes a pointer to a list as the first parameter",
            ))
        })),
    ));

    natives.push((
        String::from("tuple.get"), // tuple get element
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("tuple.get requires exactly two arguments"));
            }

            if let Value::PtrWrapper(ptr) = &args[0] {

                if let Value::Int(index) = &args[1] {
                    if *index < 1 {
                        return Value::Error(String::from("Index must be greater than 0"));
                    }

                    let list_ptr = ptr.as_ptr() as *mut Value;
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
                "tuple.get takes a pointer and an integer as parameters",
            ))
        })),
    ));

    natives.push((
        String::from("http.get"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("http.get requires exactly one argument"))
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
        String::from("http.post"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                Value::Error(String::from("http.post requires exactly two arguments"))
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
        String::from("http.put"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                Value::Error(String::from("http.put requires exactly two arguments"))
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
        String::from("http.delete"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                Value::Error(String::from("http.delete requires exactly one argument"))
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
        String::from("math.abs"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("math.abs requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.abs()),
                Value::LFloat(f) => Value::LFloat(f.abs()),
                Value::Int(i) => Value::Int(i.abs()),
                Value::BigInt(bi) => Value::BigInt(bi.abs()),
                _ => Value::Error(String::from("math.abs requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("math.sin"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("math.sin requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.sin()),
                Value::LFloat(f) => Value::LFloat(f.sin()),
                _ => Value::Error(String::from("math.sin requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("math.cos"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("math.cos requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.cos()),
                Value::LFloat(f) => Value::LFloat(f.cos()),
                _ => Value::Error(String::from("math.cos requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("math.tan"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("math.tan requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.tan()),
                Value::LFloat(f) => Value::LFloat(f.tan()),
                _ => Value::Error(String::from("math.tan requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("math.asin"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("math.asin requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.asin()),
                Value::LFloat(f) => Value::LFloat(f.asin()),
                _ => Value::Error(String::from("math.asin requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("math.acos"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("math.acos requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.acos()),
                Value::LFloat(f) => Value::LFloat(f.acos()),
                _ => Value::Error(String::from("math.acos requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("math.atan"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("math.atan requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.atan()),
                Value::LFloat(f) => Value::LFloat(f.atan()),
                _ => Value::Error(String::from("math.atan requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("math.sinh"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("math.sinh requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.sinh()),
                Value::LFloat(f) => Value::LFloat(f.sinh()),
                _ => Value::Error(String::from("math.sinh requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("math.cosh"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("math.cosh requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.cosh()),
                Value::LFloat(f) => Value::LFloat(f.cosh()),
                _ => Value::Error(String::from("math.cosh requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("math.tanh"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("math.tanh requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.tanh()),
                Value::LFloat(f) => Value::LFloat(f.tanh()),
                _ => Value::Error(String::from("math.tanh requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("math.asinh"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("math.asinh requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.asinh()),
                Value::LFloat(f) => Value::LFloat(f.asinh()),
                _ => Value::Error(String::from("math.asinh requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("math.acosh"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("math.acosh requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.acosh()),
                Value::LFloat(f) => Value::LFloat(f.acosh()),
                _ => Value::Error(String::from("math.acosh requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("math.atanh"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("math.atanh requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.atanh()),
                Value::LFloat(f) => Value::LFloat(f.atanh()),
                _ => Value::Error(String::from("math.atanh requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("math.atan2"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("math.atan2 requires exactly one argument"));
            }

            match (&args[0], &args[1]) {
                (Value::Float(f), Value::Float(f2)) => Value::Float(f.atan2(*f2)),
                (Value::LFloat(f), Value::LFloat(f2)) => Value::LFloat(f.atan2(*f2)),
                _ => Value::Error(String::from("math.atan2 requires comparible numeric arguments")),
            }
        })),
    ));

    natives.push((
        String::from("math.exp"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("math.exp requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.exp()),
                Value::LFloat(f) => Value::LFloat(f.exp()),
                _ => Value::Error(String::from("math.exp requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("math.log"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("math.log requires exactly two arguments"));
            }

            match (&args[0], &args[1]) {
                (Value::Float(x), Value::Float(base)) => Value::Float(x.log(*base)),
                (Value::LFloat(x), Value::LFloat(base)) => Value::LFloat(x.log(*base)),
                _ => Value::Error(String::from("math.log requires numeric arguments")),
            }
        })),
    ));

    natives.push((
        String::from("math.sqrt"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("math.sqrt requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.sqrt()),
                Value::LFloat(f) => Value::LFloat(f.sqrt()),
                _ => Value::Error(String::from("math.sqrt requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("math.floor"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("math.floor requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.floor()),
                Value::LFloat(f) => Value::LFloat(f.floor()),
                _ => Value::Error(String::from("math.floor requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("sys.platform"),
        Function::Native(Arc::new(|args| {
            if args.len() != 0 {
                return Value::Error(String::from("sys.platform requires exactly zero arguments"));
            }

            return Value::String(std::env::consts::OS.to_string());
        })),
    ));

    natives.push((
        String::from("math.ceil"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("math.ceil requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.ceil()),
                Value::LFloat(f) => Value::LFloat(f.ceil()),
                _ => Value::Error(String::from("math.ceil requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("math.round"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("math.round requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.round()),
                Value::LFloat(f) => Value::LFloat(f.round()),
                _ => Value::Error(String::from("math.round requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("math.trunc"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("math.trunc requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.trunc()),
                Value::LFloat(f) => Value::LFloat(f.trunc()),
                _ => Value::Error(String::from("math.trunc requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("math.pow"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("math.pow requires exactly two arguments"));
            }

            match (&args[0], &args[1]) {
                (Value::Float(x), Value::Float(y)) => Value::Float(x.powf(*y)),
                (Value::LFloat(x), Value::LFloat(y)) => Value::LFloat(x.powf(*y)),
                _ => Value::Error(String::from("math.pow requires numeric arguments")),
            }
        })),
    ));

    natives.push((
        String::from("math.exp"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("math.exp requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.exp()),
                Value::LFloat(f) => Value::LFloat(f.exp()),
                _ => Value::Error(String::from("math.exp requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("math.ln"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("math.ln requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => {
                    if *f > 0.0 {
                        Value::Float(f.ln())
                    } else {
                        Value::Error(String::from("math.ln requires a positive numeric argument"))
                    }
                }
                Value::LFloat(f) => {
                    if *f > 0.0 {
                        Value::LFloat(f.ln())
                    } else {
                        Value::Error(String::from("math.ln requires a positive numeric argument"))
                    }
                }
                _ => Value::Error(String::from("math.ln requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("math.log10"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("math.log10 requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => {
                    if *f > 0.0 {
                        Value::Float(f.log10())
                    } else {
                        Value::Error(String::from("math.log10 requires a positive numeric argument"))
                    }
                }
                Value::LFloat(f) => {
                    if *f > 0.0 {
                        Value::LFloat(f.log10())
                    } else {
                        Value::Error(String::from("math.log10 requires a positive numeric argument"))
                    }
                }
                _ => Value::Error(String::from("math.log10 requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("math.log2"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("math.log2 requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => {
                    if *f > 0.0 {
                        Value::Float(f.log10())
                    } else {
                        Value::Error(String::from("math.log2 requires a positive numeric argument"))
                    }
                }
                Value::LFloat(f) => {
                    if *f > 0.0 {
                        Value::LFloat(f.log2())
                    } else {
                        Value::Error(String::from("math.log2 requires a positive numeric argument"))
                    }
                }
                _ => Value::Error(String::from("math.log2 requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("math.pow"),
        Function::Native(Arc::new(|args| {
            if args.len() != 2 {
                return Value::Error(String::from("math.pow requires exactly two arguments"));
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
                _ => Value::Error(String::from("math.pow requires numeric arguments")),
            }
        })),
    ));

    natives.push((
        String::from("math.round"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("math.round requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(f.round()),
                Value::LFloat(f) => Value::LFloat(f.round()),
                _ => Value::Error(String::from("math.round requires a numeric argument")),
            }
        })),
    ));

    natives.push((
        String::from("math.factorial"),
        Function::Native(Arc::new(|args| {
            if args.len() != 1 {
                return Value::Error(String::from("math.factorial requires exactly one argument"));
            }

            match &args[0] {
                Value::Float(f) => Value::Float(factorial(*f)),
                Value::LFloat(f) => Value::LFloat(factorial(*f)),
                Value::BigInt(i) => Value::BigInt(factorial(*i)),
                Value::Int(i) => Value::Int(factorial(*i)),
                _ => Value::Error(String::from("math.factorial requires a numeric argument")),
            }
        })),
    ));

    natives
}
