use crate::{
    class::Class,
    function::{get_natives, Function, FunctionStruct},
    gcwrapper::GCWrapper, binary::{asm, asmtokens::tokenize, asmparser::Parser},
};
use ansi_term::Color;
use fxhash::FxHashMap;
use std::{
    collections::VecDeque,
    fmt::{Debug, Display},
    io::{stderr, stdin, stdout, Write},
    sync::Arc,
    path::PathBuf,
    fs::canonicalize,
    os::raw::{c_int, c_long, c_double, c_float, c_char}, ffi::{CString, CStr},
};
use ansi_term::Colour::{Blue, Green, White, Yellow};

fn colorize_string(input: &str) -> String {
    let mut result = String::new();
    let mut is_inside_quotes = false;
    let mut is_inside_parentheses = false;
    let mut is_space_colored = false;

    for c in input.chars() {
        match c {
            '"' => {
                result.push_str(&Blue.paint("\"").to_string());
                is_inside_quotes = !is_inside_quotes;
            }
            '(' => {
                result.push_str(&White.paint("(").to_string());
                is_inside_parentheses = true;
            }
            ')' => {
                result.push_str(&White.paint(")").to_string());
                is_inside_parentheses = false;
            }
            ' ' => {
                if is_inside_quotes || is_inside_parentheses {
                    result.push(' ');
                } else if !is_space_colored {
                    result.push_str(&White.paint(" ").to_string());
                    is_space_colored = true;
                }
            }
            _ => {
                let colored_char = match c {
                    '0'..='9' => Green.paint(c.to_string()).to_string(),
                    _ if is_inside_quotes => Blue.paint(c.to_string()).to_string(),
                    _ if !is_inside_parentheses => Yellow.paint(c.to_string()).to_string(),
                    _ => White.paint(c.to_string()).to_string(),
                };
                result.push_str(&colored_char);
                is_space_colored = false;
            }
        }
    }

    result
}

use ansi_term::{ANSIGenericString, Style};

fn underline_filename_line_column<'a>(s: String) -> ANSIGenericString<'a, str> {
    let parts: Vec<&str> = s.split(':').collect();
    if parts.len() >= 3 {
        let filename = parts[0];
        let line = parts[1];
        let column = parts[2];
        let bytes = format!(
            "{}:{}:{}: ",
            Style::new().underline().paint(filename),
            Style::new().underline().paint(line),
            Style::new().underline().paint(column)
        );
        Color::White.paint(bytes + parts.into_iter().skip(3).collect::<Vec<_>>().join("").as_str())
    } else {
        s.into()
    }
}

pub fn value_to_raw(value: Value) -> RawValue {
    match value {
        Value::None => RawValue::None,
        Value::Int(i) => RawValue::Int(i),
        Value::BigInt(i) => RawValue::BigInt(i),
        Value::Float(f) => RawValue::Float(f),
        Value::LFloat(f) => RawValue::LFloat(f),
        Value::String(s) => {
            let c_str = match std::ffi::CString::new(s) {
                Ok(c_str) => c_str,
                Err(_) => panic!("Failed to convert string to CString"),
            };
            RawValue::String(c_str.into_raw())
        }
        Value::Character(c) => RawValue::Character(c as i8),
        Value::Boolean(b) => RawValue::Boolean(b),
        Value::Error(e) => {
            let c_str = match std::ffi::CString::new(e) {
                Ok(c_str) => c_str,
                Err(_) => panic!("Failed to convert error string to CString"),
            };
            RawValue::Error(c_str.into_raw())
        }
        Value::PtrWrapper(p) => {
            if p.is_null() {
                return RawValue::RawPtr(std::ptr::null());
            }
            return RawValue::RawPtr(&value_to_raw(unsafe { p.as_ref().unwrap().clone() }) as *const RawValue);
        },
        _ => panic!("Unsupported Value variant"),
    }
}

pub fn raw_to_value(raw_value: RawValue) -> Value {
    match raw_value {
        RawValue::None => Value::None,
        RawValue::Int(i) => Value::Int(i),
        RawValue::BigInt(i) => Value::BigInt(i),
        RawValue::Float(f) => Value::Float(f),
        RawValue::LFloat(f) => Value::LFloat(f),
        RawValue::String(s) => {
            let c_str = unsafe { std::ffi::CString::from_raw(s as *mut c_char) };
            let string_value = match c_str.into_string() {
                Ok(s) => s,
                Err(_) => panic!("Failed to convert CString to string"),
            };
            Value::String(string_value)
        }
        RawValue::Character(c) => Value::Character(c as u8 as char),
        RawValue::Boolean(b) => Value::Boolean(b),
        RawValue::Error(e) => {
            let c_str = unsafe { std::ffi::CString::from_raw(e as *mut c_char) };
            let error_value = match c_str.into_string() {
                Ok(s) => s,
                Err(_) => panic!("Failed to convert CString to error string"),
            };
            Value::Error(error_value)
        }
        RawValue::RawPtr(p) => Value::RawCValueWrapper(RawValue::RawPtr(p)),
    }
}

#[derive(Clone, PartialEq)]
pub enum RawValue {
    None,
    Int(c_int),
    BigInt(c_long),
    Float(c_double),
    LFloat(c_float),
    String(*const c_char),
    Character(c_char),
    Boolean(bool),
    Error(*const c_char),
    RawPtr(*const RawValue),
}

#[derive(Clone, PartialEq)]
pub enum Value {
    None,
    Class(Class),
    Int(i32),
    BigInt(i64),
    Float(f64),
    LFloat(f32),
    String(String),
    Character(char),
    Boolean(bool),
    List(Vec<Value>),
    Tuple(Vec<Value>),
    Uninitialized,
    Error(String),
    PtrWrapper(*mut Value),
    RawCValueWrapper(RawValue),
}

#[derive(Clone, PartialEq, Debug)]
pub enum Types {
    None,
    Class,
    Int,
    BigInt,
    Float,
    LFloat,
    String,
    Character,
    Boolean,
    List,
    Tuple,
    Uninitialized,
    Error,
    PtrWrapper,
    Any,
}

pub fn value_to_readable(val: &Value) -> String {
    match val {
        Value::None => "None".to_string(),
        Value::Int(_) => "int".to_string(),
        Value::BigInt(_) => "bigint".to_string(),
        Value::Float(_) => "float".to_string(),
        Value::LFloat(_) => "lfloat".to_string(),
        Value::String(_) => "string".to_owned(),
        Value::Character(_) => "char".to_string(),
        Value::List(_) => format!("{:?}", "array"),
        Value::Tuple(_) => format!("{:?}", "tuple"),
        Value::Uninitialized => "Uninitialized".to_string(),
        Value::Boolean(_) => "boolean".to_string(),
        Value::Error(_) => "Error".to_string(),
        Value::Class(p) => format!("<class Type at {:?}>", p as *const Class),
        Value::PtrWrapper(ptr) => format!("{:#?}", ptr),
        Value::RawCValueWrapper(p) => format!("<raw_c_value_wrapper Type at {:?}>", p as *const RawValue),
    }
}

pub fn value_to_string(val: &Value) -> String {
    match val {
        Value::None => "None".to_string(),
        Value::Int(i) => i.to_string(),
        Value::BigInt(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::LFloat(f) => f.to_string(),
        Value::String(s) => s.to_owned(),
        Value::Character(c) => c.to_string(),
        Value::List(l) => {
            let elements: Vec<String> = l.iter().map(value_to_string).collect();
            format!("[{}]", elements.join(", "))
        }
        Value::Tuple(t) => {
            let elements: Vec<String> = t.iter().map(value_to_string).collect();
            format!("({})", elements.join(", "))
        }
        Value::Uninitialized => "<Uninitialized>".to_string(),
        Value::Boolean(b) => b.to_string(),
        Value::Error(e) => format!("Error: {}", e),
        Value::Class(p) => format!("<class Type at {:?}>", p as *const Class),
        Value::PtrWrapper(ptr) => {
            if ptr.is_null() {
                "<NULLPTR>".to_string()
            } else {
                format!("{:#?}", ptr)
            }
        },
        Value::RawCValueWrapper(ptr) => {
            let normal = raw_to_value(ptr.clone());
            value_to_string(&normal)
        }
    }
}

pub fn type_check_value(values: Vec<Value>, types: Vec<(String, Types)>) -> Result<(), String> {
    if values.len() != types.len() {
        return Err(format!(
            "The length of the function signature and the obtained arguments doesn't match. Expected: {}, got: {}", types.len(), values.len(),
        ));
    }
    for (value, type_) in values.into_iter().zip(types.into_iter()) {
        match (value, type_) {
            (Value::Int(_), (_, Types::Int)) => {
                continue;
            }
            (Value::BigInt(_), (_, Types::BigInt)) => {
                continue;
            }
            (Value::Float(_), (_, Types::Float)) => {
                continue;
            }
            (Value::LFloat(_), (_, Types::LFloat)) => {
                continue;
            }
            (Value::String(_), (_, Types::String)) => {
                continue;
            }
            (Value::Character(_), (_, Types::Character)) => {
                continue;
            }
            (Value::Boolean(_), (_, Types::Boolean)) => {
                continue;
            }
            (Value::List(_), (_, Types::List)) => {
                continue;
            }
            (Value::Tuple(_), (_, Types::Tuple)) => {
                continue;
            }
            (Value::Uninitialized, (_, Types::Uninitialized)) => {
                continue;
            }
            (Value::Error(_), (_, Types::Error)) => {
                continue;
            }
            (_, (_, Types::Any)) => {
                continue;
            }
            _ => {
                return Err(String::from("Type mismatch."));
            }
        }
    }
    Ok(())
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", value_to_string(self))
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", value_to_string(self))
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Instruction {
    Declare(String, Value),       // declare variable with name && val
    DeclareStatic(String, Value), // declare variable with name && val
    GetInput,                     // variable to store value in
    Print,                        // prints top value to the stdout
    Flush,                        // flushes the stdout
    PrintErr,                     // prints to the stderr
    FlushErr,                     // flushes the stderr
    Assign(String, Value),        // assigns value to already existing variable
    AssignTop(String), // assigns value at the top of the stack to already existing variable
    ThrowError(String), // throws a new error
    Push(Value),       // pushes a value to the stack
    Pop(String),       // pops the top value of the stack to a variable
    Load(String),      // loads a variable's value onto the stack
    Jump(String),      // jumps to a label
    Label(String),     // defines a label
    JumpStack(String), // jumps to a label if the top value of the stack is true
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    And,
    Or,
    Xor,
    Not,
    Eq,
    Ne,
    Gt,
    Lt,
    Gte,
    Lte,
    Catch,
    EndCatch,
    IncludeStd,     // includes the std library
    Include(String),
    Invoke(String), // invokes a function
    ToArgsStack,    // pops a value from the stack and pushes it onto the args stack
    Return,
    GetClassProperty(String, String), // from class X gets property Y.
    InvokeClassMethod(String, String), // calls a method from a class.
    SetClassProperty(String, String, Value), // at class X sets property Y to value Z.
    ClassHasProperty(String, String), // class X contains key Y
    ClassHasStaticMethod(String, String), // class X contains method Y
    HaltFromStack,
    StartFunction(String, Vec<(String, Types)>, Types),
    EndFunction,
    GCMap,
    GCTake,
    ForgetVariable(String), // LEAKS THE VARIABLE BY NOT COLLECTING IT
    Dealloc(String),
    Duplicate,
    LoopStart,
    LoopEnd,
    InlineAssembly(String),
    DereferenceRaw,
    DebuggingPrintStack,
    MemoryReadVolatile(Value),
    MemoryWriteVolatile(Value, Value),
}

pub struct VirtualMachine {
    stack: Vec<Value>,
    labels: FxHashMap<String, u32>,
    instructions: VecDeque<Instruction>,
    pc: i64,
    pub variables: GCWrapper<Value>,
    pub functions: FxHashMap<String, (Function, bool)>,
    pub classes: FxHashMap<String, Class>,
    pub included: Vec<PathBuf>,
    loop_addresses: Vec<i64>,
    is_catching: bool,
    returns_to: Vec<i64>,
    args_stack: VecDeque<Value>,
    block_len: i64,
}

impl VirtualMachine {
    pub fn new(mut instructions: Vec<Instruction>, filepath: &String) -> Result<Self, String> {
        instructions.push(Instruction::HaltFromStack);
        let mut includeds = Vec::new();
        let res = canonicalize(filepath);
        if let Err(err) = res {
            return Err(format!("{}", err))
        } else if let Ok(included) = res {
            includeds.push(included);
            Ok(Self {
                stack: vec![],
                args_stack: VecDeque::new(),
                labels: FxHashMap::default(),
                instructions: instructions.into(),
                pc: -1,
                loop_addresses: Vec::new(),
                variables: GCWrapper::new(),
                functions: FxHashMap::default(),
                classes: FxHashMap::default(),
                included: includeds,
                is_catching: false,
                returns_to: vec![],
                block_len: 0,
            })
        } else {
            unreachable!()
        }
    }

    pub fn check_labels(&mut self) {
        let mut pos = -1;
        for instruction in &self.instructions {
            pos += 1;
            assert!(pos > -1);
            match instruction {
                Instruction::Label(name) => {
                    self.labels.insert(name.clone(), pos as u32);
                    continue;
                }
                _ => continue,
            }
        }
    }

    pub fn extend_functions_wextern(
        &mut self,
        funcs: &'static Vec<(
            String,
            Arc<(dyn for<'a> Fn(&'a [Value]) -> Value + 'static)>,
        )>,
    ) {
        for (name, func) in funcs {
            self.functions.insert(
                name.to_string(),
                (Function::Native(Arc::new(|args| func(args))), true),
            );
        }
    }

    pub fn run(&mut self) -> Result<Value, String> {
        while self.pc < (self.instructions.len() - 1).try_into().unwrap() {
            self.pc += 1;
            match &self.instructions[self.pc as usize] {
                Instruction::Declare(name, value) => {
                    if self.variables.contains_key(name) {
                        if self.is_catching {
                            self.stack.push(Value::Error(format!("Redefinition of variable: Tried to redefine the variable {}, which is already defined.", name)));
                            continue;
                        } else {
                            return Err(format!("Redefinition of variable: Tried to redefine the variable {}, which is already defined.", name));
                        }
                    }
                    self.variables
                        .insert_with_status(name.clone(), value.clone());
                }
                Instruction::DeclareStatic(name, value) => {
                    if self.variables.contains_key(name) {
                        if self.is_catching {
                            self.stack.push(Value::Error(format!("Redefinition of variable: Tried to redefine the variable {} as 'static, which is already defined.", name)));
                            continue;
                        } else {
                            return Err(format!("Redefinition of variable: Tried to redefine the variable {} as 'static, which is already defined.", name));
                        }
                    }
                    self.variables.insert_static(name.clone(), value.clone());
                }
                Instruction::GetInput => {
                    let mut buffer = String::new();
                    let input = stdin().read_line(&mut buffer);
                    match input {
                        Ok(_) => {
                            self.stack.push(Value::String(buffer));
                        }
                        Err(e) => {
                            if self.is_catching {
                                self.stack
                                    .push(Value::Error(format!("Could not get input: {}", e)));
                                continue;
                            } else {
                                return Err(format!("Could not get input: {}", e));
                            }
                        }
                    }
                }
                Instruction::Print => {
                    let value_to_print = self.stack.pop();
                    match value_to_print {
                        Some(value) => match value {
                            Value::String(string) => {
                                let result = stdout().write_all(string.as_bytes());
                                if let Err(error) = result {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "Could not write to stdout: {}",
                                            error
                                        )));
                                    } else {
                                        return Err(format!(
                                            "Could not write to stdout: {}",
                                            error
                                        ));
                                    }
                                } else {
                                    continue;
                                }
                            }
                            _ => {
                                let result = stdout().write_all(value_to_string(&value).as_bytes());
                                if let Err(error) = result {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "Could not write to stdout: {}",
                                            error
                                        )));
                                    } else {
                                        return Err(format!(
                                            "Could not write to stdout: {}",
                                            error
                                        ));
                                    }
                                } else {
                                    continue;
                                }
                            }
                        },
                        None => {
                            if self.is_catching {
                                self.stack.push(Value::Error(
                                    "Could not write to stdout: The stack is empty".to_string(),
                                ))
                            } else {
                                return Err(
                                    "Could not write to stdout: The stack is empty".to_string()
                                );
                            }
                        }
                    }
                }
                Instruction::Flush => {
                    let result = stdout().flush();
                    if let Err(error) = result {
                        if self.is_catching {
                            self.stack
                                .push(Value::Error(format!("Could not flush stdout: {}", error)))
                        } else {
                            return Err(format!("Could not flush stdout: {}", error));
                        }
                    } else {
                        continue;
                    }
                }
                Instruction::PrintErr => {
                    let value_to_print = self.stack.pop();
                    match value_to_print {
                        Some(value) => match value {
                            Value::String(string) => {
                                let result = stderr().write_all(string.as_bytes());
                                if let Err(error) = result {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "Could not write to stderr: {}",
                                            error
                                        )))
                                    } else {
                                        return Err(format!(
                                            "Could not write to stderr: {}",
                                            error
                                        ));
                                    }
                                } else {
                                    continue;
                                }
                            }
                            _ => {
                                let result = stderr().write_all(value_to_string(&value).as_bytes());
                                if let Err(error) = result {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "Could not write to stderr: {}",
                                            error
                                        )))
                                    } else {
                                        return Err(format!(
                                            "Could not write to stderr: {}",
                                            error
                                        ));
                                    }
                                } else {
                                    continue;
                                }
                            }
                        },
                        None => {
                            if self.is_catching {
                                self.stack.push(Value::Error(
                                    "Could not write to stderr: The stack is empty".to_string(),
                                ))
                            } else {
                                return Err(
                                    "Could not write to stderr: The stack is empty".to_string()
                                );
                            }
                        }
                    }
                }
                Instruction::FlushErr => {
                    let result = stderr().flush();
                    if let Err(error) = result {
                        if self.is_catching {
                            self.stack
                                .push(Value::Error(format!("Could not flush stderr: {}", error)))
                        } else {
                            return Err(format!("Could not flush stderr: {}", error));
                        }
                    } else {
                        continue;
                    }
                }
                Instruction::Assign(name, value) => {
                    if !self.variables.contains_key(name) {
                        if self.is_catching {
                            self.stack.push(Value::Error(format!(
                                "Cannot assign to undefined variable {}",
                                name
                            )))
                        } else {
                            return Err(format!("Cannot assign to undefined variable {}", name));
                        }
                    } else {
                        self.variables
                            .insert_with_status(name.clone(), value.clone());
                        continue;
                    }
                }
                Instruction::AssignTop(name) => {
                    if !self.variables.contains_key(name) {
                        if self.is_catching {
                            self.stack.push(Value::Error(format!(
                                "Cannot assign to undefined variable {}",
                                name
                            )))
                        } else {
                            return Err(format!("Cannot assign to undefined variable {}", name));
                        }
                    } else {
                        let value = self.stack.pop();
                        if let Some(value) = value {
                            self.variables
                                .insert_with_status(name.clone(), value.clone());
                            continue;
                        } else if self.is_catching {
                            self.stack.push(Value::Error(
                                "Cannot assign to {} from the stack because the stack is empty"
                                    .to_string(),
                            ))
                        } else {
                            return Err(
                                "Cannot assign to {} from the stack because the stack is empty"
                                    .to_string(),
                            );
                        }
                    }
                }
                Instruction::ThrowError(err) => {
                    if !self.is_catching {
                        return Err(err.to_string());
                    } else {
                        self.stack.push(Value::Error(err.clone()))
                    }
                }
                Instruction::Push(val) => {
                    self.stack.push(val.clone());
                    continue;
                }
                Instruction::Load(name) => {
                    let value = self.variables.get(name);
                    if let Some(value) = value {
                        self.stack.push(value.clone());
                        continue;
                    } else if self.is_catching {
                        self.stack.push(Value::Error(format!(
                            "Cannot load undefined variable {}",
                            name
                        )));
                    } else {
                        return Err(format!("Cannot load undefined variable {}", name));
                    }
                }
                Instruction::Jump(label) => {
                    if let Some(line) = self.labels.get(label) {
                        self.pc = *line as i64;
                    } else if self.is_catching {
                        self.stack.push(Value::Error(format!(
                            "Cannot jump to undefined label {}",
                            label
                        )));
                    } else {
                        return Err(format!("Cannot jump to undefined label {}", label));
                    }
                }
                Instruction::JumpStack(label) => {
                    if let Some(val) = self.stack.pop() {
                        match val {
                            Value::Boolean(b) if b => {
                                if let Some(line) = self.labels.get(label) {
                                    self.pc = *line as i64;
                                } else if self.is_catching {
                                    self.stack.push(Value::Error(format!(
                                        "Cannot jump to undefined label {}",
                                        label
                                    )));
                                } else {
                                    return Err(format!(
                                        "Cannot jump to undefined label {}",
                                        label
                                    ));
                                }
                            }
                            _ => continue,
                        }
                    } else if self.is_catching {
                        self.stack.push(Value::Error(format!("Cannot jump to label {} according to the top stack value because the stack is empty", label)));
                    } else {
                        return Err(format!("Cannot jump to label {} according to the top stack value because the stack is empty", label));
                    }
                }
                Instruction::Add => {
                    if let Some(val) = self.stack.pop() {
                        if let Some(val2) = self.stack.pop() {
                            match val {
                                Value::Int(i1) => match val2 {
                                    Value::Int(i) => {
                                        self.stack.push(Value::Int(i1 + i));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Addition is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Addition is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::BigInt(i1) => match val2 {
                                    Value::BigInt(i) => {
                                        self.stack.push(Value::BigInt(i1 + i));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Addition is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Addition is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::Float(f1) => match val2 {
                                    Value::Float(f) => {
                                        self.stack.push(Value::Float(f1 + f));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Addition is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Addition is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::LFloat(f1) => match val2 {
                                    Value::LFloat(f) => {
                                        self.stack.push(Value::LFloat(f1 + f));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Addition is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Addition is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::String(s1) => match val2 {
                                    Value::String(s2) => {
                                        self.stack.push(Value::String(s2 + &s1));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!(
                                                    "Addition is not supported between the types {} and {}",
                                                    "string",
                                                    value_to_readable(&val2)
                                                )));
                                        } else {
                                            return Err(format!(
                                                    "Addition is not supported between the types {} and {}",
                                                    "string",
                                                    value_to_readable(&val2)
                                                ));
                                        }
                                    }
                                },
                                _ => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!("Cannot apply the addition operator in a non-numeric value {}", value_to_string(&val))));
                                    } else {
                                        return Err(format!("Cannot apply the addition operator in a non-numeric value {}", value_to_string(&val)));
                                    }
                                }
                            }
                        } else if self.is_catching {
                            self.stack
                                .push(Value::Error(String::from("Not enough operands")));
                        } else {
                            return Err(String::from("Not enough operands"));
                        }
                    } else if self.is_catching {
                        self.stack
                            .push(Value::Error(String::from("The stack is empty")));
                    } else {
                        return Err(String::from("The stack is empty"));
                    }
                }
                Instruction::Sub => {
                    if let Some(val) = self.stack.pop() {
                        if let Some(val2) = self.stack.pop() {
                            match val {
                                Value::Int(i1) => match val2 {
                                    Value::Int(i) => {
                                        self.stack.push(Value::Int(i1 - i));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Subtraction is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Subtraction is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::BigInt(i1) => match val2 {
                                    Value::BigInt(i) => {
                                        self.stack.push(Value::BigInt(i1 - i));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Subtraction is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Subtraction is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::Float(f1) => match val2 {
                                    Value::Float(f) => {
                                        self.stack.push(Value::Float(f1 - f));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Subtraction is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Subtraction is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::LFloat(f1) => match val2 {
                                    Value::LFloat(f) => {
                                        self.stack.push(Value::LFloat(f1 - f));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Subtraction is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Subtraction is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::String(s1) => match val2 {
                                    Value::String(s2) => {
                                        if let Some(index) = s2.rfind(&s1) {
                                            let result = format!(
                                                "{}{}",
                                                &s2[..index],
                                                &s2[index + s1.len()..]
                                            );
                                            self.stack.push(Value::String(result));
                                        } else if self.is_catching {
                                            self.stack.push(Value::Error(format!(
                                                "Substring '{}' not found in '{}'",
                                                s1, s2
                                            )));
                                        } else {
                                            return Err(format!(
                                                "Substring '{}' not found in '{}'",
                                                s1, s2
                                            ));
                                        }
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!(
                                                "Subtraction is not supported between the types {} and {}",
                                                "string",
                                                value_to_readable(&val2)
                                            )));
                                        } else {
                                            return Err(format!(
                                                "Subtraction is not supported between the types {} and {}",
                                                "string",
                                                value_to_readable(&val2)
                                            ));
                                        }
                                    }
                                },
                                _ => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!("Cannot apply the subtraction operator in a non-numeric value {}", value_to_string(&val))));
                                    } else {
                                        return Err(format!("Cannot apply the addition subtraction in a non-numeric value {}", value_to_string(&val)));
                                    }
                                }
                            }
                        } else if self.is_catching {
                            self.stack
                                .push(Value::Error(String::from("Not enough operands")));
                        } else {
                            return Err(String::from("Not enough operands"));
                        }
                    } else if self.is_catching {
                        self.stack
                            .push(Value::Error(String::from("The stack is empty")));
                    } else {
                        return Err(String::from("The stack is empty"));
                    }
                }
                Instruction::Mul => {
                    if let Some(val) = self.stack.pop() {
                        if let Some(val2) = self.stack.pop() {
                            match val {
                                Value::Int(i1) => match val2 {
                                    Value::Int(i) => {
                                        self.stack.push(Value::Int(i1 * i));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Multiplication is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Multiplication is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::BigInt(i1) => match val2 {
                                    Value::BigInt(i) => {
                                        self.stack.push(Value::BigInt(i1 * i));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Multiplication is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Multiplication is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::Float(f1) => match val2 {
                                    Value::Float(f) => {
                                        self.stack.push(Value::Float(f1 * f));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Multiplication is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Multiplication is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::LFloat(f1) => match val2 {
                                    Value::LFloat(f) => {
                                        self.stack.push(Value::LFloat(f1 * f));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Multiplication is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Multiplication is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                _ => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!("Cannot apply the multiplication operator in a non-numeric value {}", value_to_string(&val))));
                                    } else {
                                        return Err(format!("Cannot apply the multiplication operator in a non-numeric value {}", value_to_string(&val)));
                                    }
                                }
                            }
                        } else if self.is_catching {
                            self.stack
                                .push(Value::Error(String::from("Not enough operands")));
                        } else {
                            return Err(String::from("Not enough operands"));
                        }
                    } else if self.is_catching {
                        self.stack
                            .push(Value::Error(String::from("The stack is empty")));
                    } else {
                        return Err(String::from("The stack is empty"));
                    }
                }
                Instruction::Div => {
                    if let Some(val) = self.stack.pop() {
                        if let Some(val2) = self.stack.pop() {
                            match val {
                                Value::Int(i1) => match val2 {
                                    Value::Int(i) => {
                                        self.stack.push(Value::Int(i1 / i));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Division is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Division is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::BigInt(i1) => match val2 {
                                    Value::BigInt(i) => {
                                        self.stack.push(Value::BigInt(i1 / i));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Division is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Division is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::Float(f1) => match val2 {
                                    Value::Float(f) => {
                                        self.stack.push(Value::Float(f1 / f));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Division is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Division is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::LFloat(f1) => match val2 {
                                    Value::LFloat(f) => {
                                        self.stack.push(Value::LFloat(f1 / f));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Division is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Division is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                _ => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!("Cannot apply the division operator in a non-numeric value {}", value_to_string(&val))));
                                    } else {
                                        return Err(format!("Cannot apply the division operator in a non-numeric value {}", value_to_string(&val)));
                                    }
                                }
                            }
                        } else if self.is_catching {
                            self.stack
                                .push(Value::Error(String::from("Not enough operands")));
                        } else {
                            return Err(String::from("Not enough operands"));
                        }
                    } else if self.is_catching {
                        self.stack
                            .push(Value::Error(String::from("The stack is empty")));
                    } else {
                        return Err(String::from("The stack is empty"));
                    }
                }
                Instruction::Mod => {
                    if let Some(val) = self.stack.pop() {
                        if let Some(val2) = self.stack.pop() {
                            match val {
                                Value::Int(i1) => match val2 {
                                    Value::Int(i) => {
                                        self.stack.push(Value::Int(i1 % i));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Remainder is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Remainder is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::BigInt(i1) => match val2 {
                                    Value::BigInt(i) => {
                                        self.stack.push(Value::BigInt(i1 % i));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Remainder is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Remainder is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::Float(f1) => match val2 {
                                    Value::Float(f) => {
                                        self.stack.push(Value::Float(f1 % f));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Remainder is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Remainder is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::LFloat(f1) => match val2 {
                                    Value::LFloat(f) => {
                                        self.stack.push(Value::LFloat(f1 % f));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Remainder is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Remainder is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                _ => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!("Cannot apply the modulo operator in a non-numeric value {}", value_to_string(&val))));
                                    } else {
                                        return Err(format!("Cannot apply the modulo operator in a non-numeric value {}", value_to_string(&val)));
                                    }
                                }
                            }
                        } else if self.is_catching {
                            self.stack
                                .push(Value::Error(String::from("Not enough operands")));
                        } else {
                            return Err(String::from("Not enough operands"));
                        }
                    } else if self.is_catching {
                        self.stack
                            .push(Value::Error(String::from("The stack is empty")));
                    } else {
                        return Err(String::from("The stack is empty"));
                    }
                }
                Instruction::Pow => {
                    if let Some(val) = self.stack.pop() {
                        if let Some(val2) = self.stack.pop() {
                            match val {
                                Value::Int(i1) => match val2 {
                                    Value::Int(i) => {
                                        if i < 1 {
                                            if self.is_catching {
                                                self.stack.push(Value::Error(format!("Exponentiation is not supported with a negative exponent {}", value_to_readable(&val2))));
                                            } else {
                                                return Err(format!("Exponentiation is not supported with a negative exponent {}", value_to_readable(&val2)));
                                            }
                                        }
                                        self.stack.push(Value::Int(i1.pow(i.try_into().unwrap())));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Power is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Power is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::BigInt(i1) => match val2 {
                                    Value::BigInt(i) => {
                                        if i < 1 {
                                            if self.is_catching {
                                                self.stack.push(Value::Error(format!("Exponentiation is not supported with a negative exponent {}", value_to_readable(&val2))));
                                            } else {
                                                return Err(format!("Exponentiation is not supported with a negative exponent {}", value_to_readable(&val2)));
                                            }
                                        }
                                        self.stack
                                            .push(Value::BigInt(i1.pow(i.try_into().unwrap())));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Power is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Power is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::Float(f1) => match val2 {
                                    Value::Float(f) => {
                                        self.stack.push(Value::Float(f1.powf(f)));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Power is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Power is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::LFloat(f1) => match val2 {
                                    Value::LFloat(f) => {
                                        self.stack.push(Value::LFloat(f1.powf(f)));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Power is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Power is not supported between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                _ => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!("Cannot apply the exponential operator in a non-numeric value {}", value_to_string(&val))));
                                    } else {
                                        return Err(format!("Cannot apply the exponential operator in a non-numeric value {}", value_to_string(&val)));
                                    }
                                }
                            }
                        } else if self.is_catching {
                            self.stack
                                .push(Value::Error(String::from("Not enough operands")));
                        } else {
                            return Err(String::from("Not enough operands"));
                        }
                    } else if self.is_catching {
                        self.stack
                            .push(Value::Error(String::from("The stack is empty")));
                    } else {
                        return Err(String::from("The stack is empty"));
                    }
                }
                Instruction::And => {
                    if let Some(val) = self.stack.pop() {
                        if let Some(val2) = self.stack.pop() {
                            match val {
                                Value::Boolean(b1) => match val2 {
                                    Value::Boolean(b2) => {
                                        self.stack.push(Value::Boolean(b1 && b2));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Cannot apply the and operator in a non-boolean value {}", value_to_string(&val2))));
                                        } else {
                                            return Err(format!("Cannot apply the and operator in a non-boolean value {}", value_to_string(&val2)));
                                        }
                                    }
                                },
                                _ => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!("Cannot apply the and operator in a non-boolean value {}", value_to_string(&val))));
                                    } else {
                                        return Err(format!("Cannot apply the and operator in a non-boolean value {}", value_to_string(&val)));
                                    }
                                }
                            }
                        } else if self.is_catching {
                            self.stack
                                .push(Value::Error(String::from("Not enough operands")));
                        } else {
                            return Err(String::from("Not enough operands"));
                        }
                    } else if self.is_catching {
                        self.stack
                            .push(Value::Error(String::from("The stack is empty")));
                    } else {
                        return Err(String::from("The stack is empty"));
                    }
                }
                Instruction::Or => {
                    if let Some(val) = self.stack.pop() {
                        if let Some(val2) = self.stack.pop() {
                            match val {
                                Value::Boolean(b1) => match val2 {
                                    Value::Boolean(b2) => {
                                        self.stack.push(Value::Boolean(b1 || b2));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Cannot apply the or operator in a non-boolean value {}", value_to_string(&val2))));
                                        } else {
                                            return Err(format!("Cannot apply the or operator in a non-boolean value {}", value_to_string(&val2)));
                                        }
                                    }
                                },
                                _ => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!("Cannot apply the or operator in a non-boolean value {}", value_to_string(&val))));
                                    } else {
                                        return Err(format!("Cannot apply the or operator in a non-boolean value {}", value_to_string(&val)));
                                    }
                                }
                            }
                        } else if self.is_catching {
                            self.stack
                                .push(Value::Error(String::from("Not enough operands")));
                        } else {
                            return Err(String::from("Not enough operands"));
                        }
                    } else if self.is_catching {
                        self.stack
                            .push(Value::Error(String::from("The stack is empty")));
                    } else {
                        return Err(String::from("The stack is empty"));
                    }
                }
                Instruction::Xor => {
                    if let Some(val) = self.stack.pop() {
                        if let Some(val2) = self.stack.pop() {
                            match val {
                                Value::Boolean(b1) => match val2 {
                                    Value::Boolean(b2) => {
                                        self.stack.push(Value::Boolean(b1 ^ b2));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Cannot apply the xor operator in a non-boolean value {}", value_to_string(&val2))));
                                        } else {
                                            return Err(format!("Cannot apply the xor operator in a non-boolean value {}", value_to_string(&val2)));
                                        }
                                    }
                                },
                                _ => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!("Cannot apply the xor operator in a non-boolean value {}", value_to_string(&val))));
                                    } else {
                                        return Err(format!("Cannot apply the xor operator in a non-boolean value {}", value_to_string(&val)));
                                    }
                                }
                            }
                        } else if self.is_catching {
                            self.stack
                                .push(Value::Error(String::from("Not enough operands")));
                        } else {
                            return Err(String::from("Not enough operands"));
                        }
                    } else if self.is_catching {
                        self.stack
                            .push(Value::Error(String::from("The stack is empty")));
                    } else {
                        return Err(String::from("The stack is empty"));
                    }
                }
                Instruction::Not => {
                    if let Some(val) = self.stack.pop() {
                        match val {
                            Value::Boolean(b1) => {
                                self.stack.push(Value::Boolean(!b1));
                            }
                            _ => {
                                if self.is_catching {
                                    self.stack.push(Value::Error(format!(
                                        "Cannot apply the not operator in a non-boolean value {}",
                                        value_to_string(&val)
                                    )));
                                } else {
                                    return Err(format!(
                                        "Cannot apply the not operator in a non-boolean value {}",
                                        value_to_string(&val)
                                    ));
                                }
                            }
                        }
                    }
                }
                Instruction::Eq => {
                    if let Some(val) = self.stack.pop() {
                        if let Some(val2) = self.stack.pop() {
                            self.stack.push(Value::Boolean(val == val2));
                        } else if self.is_catching {
                            self.stack
                                .push(Value::Error(String::from("Not enough operands")));
                        } else {
                            return Err(String::from("Not enough operands"));
                        }
                    } else if self.is_catching {
                        self.stack
                            .push(Value::Error(String::from("The stack is empty")));
                    } else {
                        return Err(String::from("The stack is empty"));
                    }
                }
                Instruction::Ne => {
                    if let Some(val) = self.stack.pop() {
                        if let Some(val2) = self.stack.pop() {
                            self.stack.push(Value::Boolean(val != val2));
                        } else if self.is_catching {
                            self.stack
                                .push(Value::Error(String::from("Not enough operands")));
                        } else {
                            return Err(String::from("Not enough operands"));
                        }
                    } else if self.is_catching {
                        self.stack
                            .push(Value::Error(String::from("The stack is empty")));
                    } else {
                        return Err(String::from("The stack is empty"));
                    }
                }
                Instruction::Gt => {
                    if let Some(val) = self.stack.pop() {
                        if let Some(val2) = self.stack.pop() {
                            match val {
                                Value::Int(b1) => match val2 {
                                    Value::Int(b2) => {
                                        self.stack.push(Value::Boolean(b1 > b2));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Cannot apply the greater than operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Cannot apply the greater than operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::BigInt(b1) => match val2 {
                                    Value::BigInt(b2) => {
                                        self.stack.push(Value::Boolean(b1 > b2));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Cannot apply the greater than operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Cannot apply the greater than operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::Float(b1) => match val2 {
                                    Value::Float(b2) => {
                                        self.stack.push(Value::Boolean(b1 > b2));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Cannot apply the greater than operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Cannot apply the greater than operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::LFloat(b1) => match val2 {
                                    Value::LFloat(b2) => {
                                        self.stack.push(Value::Boolean(b1 > b2));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Cannot apply the greater than operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Cannot apply the greater than operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                _ => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!("Cannot apply the greater than operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                    } else {
                                        return Err(format!("Cannot apply the greater than operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                    }
                                }
                            }
                        } else if self.is_catching {
                            self.stack
                                .push(Value::Error(String::from("Not enough operands")));
                        } else {
                            return Err(String::from("Not enough operands"));
                        }
                    } else if self.is_catching {
                        self.stack
                            .push(Value::Error(String::from("The stack is empty")));
                    } else {
                        return Err(String::from("The stack is empty"));
                    }
                }
                Instruction::Lt => {
                    if let Some(val) = self.stack.pop() {
                        if let Some(val2) = self.stack.pop() {
                            match val {
                                Value::Int(b1) => match val2 {
                                    Value::Int(b2) => {
                                        self.stack.push(Value::Boolean(b1 < b2));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Cannot apply the less than operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Cannot apply the less than operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::BigInt(b1) => match val2 {
                                    Value::BigInt(b2) => {
                                        self.stack.push(Value::Boolean(b1 < b2));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Cannot apply the less than operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Cannot apply the less than operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::Float(b1) => match val2 {
                                    Value::Float(b2) => {
                                        self.stack.push(Value::Boolean(b1 < b2));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Cannot apply the less than operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Cannot apply the less than operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::LFloat(b1) => match val2 {
                                    Value::LFloat(b2) => {
                                        self.stack.push(Value::Boolean(b1 < b2));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Cannot apply the less than operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Cannot apply the less than operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                _ => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!("Cannot apply the less than operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                    } else {
                                        return Err(format!("Cannot apply the less than operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                    }
                                }
                            }
                        } else if self.is_catching {
                            self.stack
                                .push(Value::Error(String::from("Not enough operands")));
                        } else {
                            return Err(String::from("Not enough operands"));
                        }
                    } else if self.is_catching {
                        self.stack
                            .push(Value::Error(String::from("The stack is empty")));
                    } else {
                        return Err(String::from("The stack is empty"));
                    }
                }
                Instruction::Gte => {
                    if let Some(val) = self.stack.pop() {
                        if let Some(val2) = self.stack.pop() {
                            match val {
                                Value::Int(b1) => match val2 {
                                    Value::Int(b2) => {
                                        self.stack.push(Value::Boolean(b1 >= b2));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Cannot apply the greater than equal operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Cannot apply the greater than equal operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::BigInt(b1) => match val2 {
                                    Value::BigInt(b2) => {
                                        self.stack.push(Value::Boolean(b1 >= b2));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Cannot apply the greater than equal operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Cannot apply the greater than equal operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::Float(b1) => match val2 {
                                    Value::Float(b2) => {
                                        self.stack.push(Value::Boolean(b1 >= b2));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Cannot apply the greater than equal operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Cannot apply the greater than equal operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::LFloat(b1) => match val2 {
                                    Value::LFloat(b2) => {
                                        self.stack.push(Value::Boolean(b1 >= b2));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Cannot apply the greater than equal operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Cannot apply the greater than equal operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                _ => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!("Cannot apply the greater than equal operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                    } else {
                                        return Err(format!("Cannot apply the greater than equal operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                    }
                                }
                            }
                        } else if self.is_catching {
                            self.stack
                                .push(Value::Error(String::from("Not enough operands")));
                        } else {
                            return Err(String::from("Not enough operands"));
                        }
                    } else if self.is_catching {
                        self.stack
                            .push(Value::Error(String::from("The stack is empty")));
                    } else {
                        return Err(String::from("The stack is empty"));
                    }
                }
                Instruction::Lte => {
                    if let Some(val) = self.stack.pop() {
                        if let Some(val2) = self.stack.pop() {
                            match val {
                                Value::Int(b1) => match val2 {
                                    Value::Int(b2) => {
                                        self.stack.push(Value::Boolean(b1 <= b2));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Cannot apply the less than equal operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Cannot apply the less than equal operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::BigInt(b1) => match val2 {
                                    Value::BigInt(b2) => {
                                        self.stack.push(Value::Boolean(b1 <= b2));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Cannot apply the less than equal operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Cannot apply the less than equal operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::Float(b1) => match val2 {
                                    Value::Float(b2) => {
                                        self.stack.push(Value::Boolean(b1 <= b2));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Cannot apply the less than equal operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Cannot apply the less than equal operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                Value::LFloat(b1) => match val2 {
                                    Value::LFloat(b2) => {
                                        self.stack.push(Value::Boolean(b1 <= b2));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!("Cannot apply the less than equal operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                        } else {
                                            return Err(format!("Cannot apply the less than equal operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                        }
                                    }
                                },
                                _ => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!("Cannot apply the less than equal operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2))));
                                    } else {
                                        return Err(format!("Cannot apply the less than equal operator between the types {} and {}", value_to_readable(&val), value_to_readable(&val2)));
                                    }
                                }
                            }
                        } else if self.is_catching {
                            self.stack
                                .push(Value::Error(String::from("Not enough operands")));
                        } else {
                            return Err(String::from("Not enough operands"));
                        }
                    } else if self.is_catching {
                        self.stack
                            .push(Value::Error(String::from("The stack is empty")));
                    } else {
                        return Err(String::from("The stack is empty"));
                    }
                }
                Instruction::Label(_) => continue,
                Instruction::Catch => {
                    self.is_catching = true;
                }
                Instruction::EndCatch => {
                    self.is_catching = false;
                }
                Instruction::IncludeStd => {
                    let natives = get_natives();
                    for (key, func) in natives {
                        self.functions.insert(key, (func, false));
                    }
                }
                Instruction::Invoke(name) => {
                    if !self.functions.contains_key(name) {
                        if self.is_catching {
                            self.stack.push(Value::Error(format!(
                                "Cannot invoke undefined function {}",
                                name
                            )));
                        } else {
                            return Err(format!("Cannot invoke undefined function {}", name));
                        }
                    } else {
                        let func = self.functions.get(name).unwrap();
                        match func {
                            (Function::Native(fun), isextern) => {
                                if !isextern {
                                    let mut args: Vec<Value> = vec![];
                                    while let Some(value) = self.args_stack.pop_front() {
                                        args.push(value);
                                    }
                                    let result = fun(args.as_slice());
                                    match result {
                                        Value::Error(e) => {
                                            if self.is_catching {
                                                self.stack.push(Value::Error(e));
                                            } else {
                                                return Err(e);
                                            }
                                        }
                                        _ => {
                                            self.stack.push(result);
                                        }
                                    }
                                } else {
                                    let mut args: Vec<Value> = vec![];
                                    while let Some(value) = self.args_stack.pop_front() {
                                        args.push(value);
                                    }
                                    let result = fun(args.as_slice());
                                    match result {
                                        Value::Error(e) => {
                                            if self.is_catching {
                                                self.stack.push(Value::Error(e));
                                            } else {
                                                return Err(e);
                                            }
                                        }
                                        _ => {
                                            self.stack.push(result);
                                        }
                                    }
                                }
                            }
                            (Function::Interpreted(fun), _) => {
                                let mut args: VecDeque<Value> = VecDeque::new();
                                if self.args_stack.len() != fun.args.len() {
                                    if self.is_catching {
                                        self.stack
                                            .push(Value::Error(String::from("The argument stack cannot fulfil the size of the function's arguments.")));
                                    } else {
                                        return Err(String::from("The argument stack cannot fulfil the size of the function's arguments."));
                                    }
                                }
                                if fun.args.len() != 0 {
                                    for _ in 0..fun.args.len() - 1 {
                                        args.push_back(self.args_stack.pop_front().unwrap());
                                    }
                                }
                                args.append(&mut self.args_stack);
                                let typechecks = type_check_value(args.clone().into(), fun.args.clone());
                                if let Err(err) = typechecks {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(err));
                                    } else {
                                        return Err(err);
                                    }
                                } else {
                                    self.returns_to.push(self.pc + 3);
                                    let fun_body_len = fun.body.len();
                                    self.instructions
                                        .drain(self.pc as usize..(self.pc as usize));
                                    let other_part = self
                                        .instructions
                                        .drain((self.pc as usize)..)
                                        .collect::<Vec<Instruction>>();
                                    self.instructions.push_back(Instruction::GCMap);
                                    self.instructions
                                        .append(&mut VecDeque::from(fun.body.clone()));
                                    self.instructions.push_back(Instruction::GCTake);
                                    self.instructions.append(&mut VecDeque::from(other_part));
                                    self.block_len = fun_body_len as i64;
                                    let zipped = fun.args.clone().into_iter();
                                    let zipped = zipped.zip(args.into_iter());
                                    for ((varname, _), value) in zipped {
                                        self.variables.insert(varname, value);
                                    }
                                }
                            }
                        }
                    }
                }
                Instruction::Pop(name) => {
                    let top = self.stack.pop();
                    if let Some(top) = top {
                        self.variables.insert_with_status(name.to_string(), top);
                    } else if self.is_catching {
                        self.stack
                            .push(Value::Error(String::from("The stack is empty")));
                    } else {
                        return Err(String::from("The stack is empty"));
                    }
                }
                Instruction::ToArgsStack => {
                    let top = self.stack.pop();
                    if let Some(top) = top {
                        self.args_stack.push_back(top);
                    } else if self.is_catching {
                        self.stack
                            .push(Value::Error(String::from("The stack is empty")));
                    } else {
                        return Err(String::from("The stack is empty"));
                    }
                }
                Instruction::Return => {
                    let temp = self.returns_to.pop();
                    match temp {
                        Some(returnsto) => {
                            self.pc = returnsto;
                            self.instructions.drain(
                                (self.pc as usize + 1)..((self.pc + self.block_len) as usize),
                            );
                        }
                        None => {
                            self.pc = (self.instructions.len() - 1) as i64;
                        }
                    }
                }
                Instruction::GetClassProperty(classname, property) => {
                    if !self.variables.contains_key(classname) {
                        if self.is_catching {
                            self.stack.push(Value::Error(format!(
                                "Cannot use undefined variable {}",
                                classname
                            )));
                        } else {
                            return Err(format!("Cannot use undefined variable {}", classname));
                        }
                    } else {
                        let class = self.variables.get(classname).unwrap();
                        match class {
                            Value::Class(class) => {
                                if !class.properties.contains_key(property) {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "Class {} doesn't have property {}",
                                            class.name, property
                                        )));
                                    } else {
                                        return Err(format!(
                                            "Class {} doesn't have property {}",
                                            class.name, property
                                        ));
                                    }
                                } else {
                                    self.stack
                                        .push(class.properties.get(property).unwrap().clone());
                                }
                            }
                            _ => {
                                if self.is_catching {
                                    self.stack.push(Value::Error(format!(
                                        "{} is not a class",
                                        classname
                                    )));
                                } else {
                                    return Err(format!("{} is not a class", classname));
                                }
                            }
                        }
                    }
                }
                Instruction::InvokeClassMethod(classname, name) => {
                    if !self.variables.contains_key(classname) {
                        if self.is_catching {
                            self.stack.push(Value::Error(format!(
                                "Cannot use undefined variable {}",
                                classname
                            )));
                        } else {
                            return Err(format!("Cannot use undefined variable {}", classname));
                        }
                    } else {
                        let class = self.variables.get(classname).unwrap();
                        match class {
                            Value::Class(class) => {
                                if !class.staticmethods.contains_key(name) {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "{} is not a class",
                                            classname
                                        )));
                                    } else {
                                        return Err(format!("{} is not a class", classname));
                                    }
                                }
                                let func = class.staticmethods.get(name).unwrap();
                                match func {
                                    Function::Native(fun) => {
                                        let mut args: Vec<Value> = vec![];
                                        for _ in 0..(self.args_stack.len() as i64) - 1 {
                                            args.push(self.stack.pop().unwrap());
                                        }
                                        let result = fun(args.as_slice());
                                        match result {
                                            Value::Error(e) => {
                                                if self.is_catching {
                                                    self.stack.push(Value::Error(e));
                                                } else {
                                                    return Err(e);
                                                }
                                            }
                                            _ => {
                                                self.stack.push(result);
                                            }
                                        }
                                    }
                                    Function::Interpreted(fun) => {
                                        let mut args: Vec<Value> = vec![];
                                        for _ in 0..fun.args.len() - 1 {
                                            args.push(self.args_stack.pop_front().unwrap());
                                        }
                                        let typechecks = type_check_value(args, fun.args.clone());
                                        if let Err(err) = typechecks {
                                            if self.is_catching {
                                                self.stack.push(Value::Error(err));
                                            } else {
                                                return Err(err);
                                            }
                                        } else {
                                            self.returns_to.push(self.pc + 1);
                                            let fun_body_len = fun.body.len();
                                            self.instructions
                                                .drain(self.pc as usize..(self.pc as usize + 1));
                                            self.instructions.extend(fun.body.iter().cloned());
                                            self.block_len = fun_body_len as i64;
                                        }
                                    }
                                }
                            }
                            _ => {
                                if self.is_catching {
                                    self.stack.push(Value::Error(format!(
                                        "{} is not a class",
                                        classname
                                    )));
                                } else {
                                    return Err(format!("{} is not a class", classname));
                                }
                            }
                        }
                    }
                }
                Instruction::SetClassProperty(classname, name, val) => {
                    if !self.variables.contains_key(classname) {
                        if self.is_catching {
                            self.stack.push(Value::Error(format!(
                                "Cannot use undefined variable {}",
                                classname
                            )));
                        } else {
                            return Err(format!("Cannot use undefined variable {}", classname));
                        }
                    } else {
                        let class = self.variables.get_mut(classname).unwrap();
                        match class {
                            Value::Class(class) => {
                                class.properties.insert(name.to_string(), val.clone());
                            }
                            _ => {
                                if self.is_catching {
                                    self.stack.push(Value::Error(format!(
                                        "{} is not a class",
                                        classname
                                    )));
                                } else {
                                    return Err(format!("{} is not a class", classname));
                                }
                            }
                        }
                    }
                }
                Instruction::ClassHasProperty(classname, property) => {
                    if !self.variables.contains_key(classname) {
                        if self.is_catching {
                            self.stack.push(Value::Error(format!(
                                "Cannot use undefined variable {}",
                                classname
                            )));
                        } else {
                            return Err(format!("Cannot use undefined variable {}", classname));
                        }
                    } else {
                        let class = self.variables.get_mut(classname).unwrap();
                        match class {
                            Value::Class(class) => {
                                self.stack
                                    .push(Value::Boolean(class.properties.contains_key(property)));
                            }
                            _ => {
                                if self.is_catching {
                                    self.stack.push(Value::Error(format!(
                                        "{} is not a class",
                                        classname
                                    )));
                                } else {
                                    return Err(format!("{} is not a class", classname));
                                }
                            }
                        }
                    }
                }
                Instruction::ClassHasStaticMethod(classname, staticmethod) => {
                    if !self.variables.contains_key(classname) {
                        if self.is_catching {
                            self.stack.push(Value::Error(format!(
                                "Cannot use undefined variable {}",
                                classname
                            )));
                        } else {
                            return Err(format!("Cannot use undefined variable {}", classname));
                        }
                    } else {
                        let class = self.variables.get_mut(classname).unwrap();
                        match class {
                            Value::Class(class) => {
                                self.stack.push(Value::Boolean(
                                    class.staticmethods.contains_key(staticmethod),
                                ));
                            }
                            _ => {
                                if self.is_catching {
                                    self.stack.push(Value::Error(format!(
                                        "{} is not a class",
                                        classname
                                    )));
                                } else {
                                    return Err(format!("{} is not a class", classname));
                                }
                            }
                        }
                    }
                }
                Instruction::HaltFromStack => {
                    let val = self.stack.pop();
                    match val {
                        Some(val) => return Ok(val),
                        None => return Ok(Value::None),
                    }
                }
                Instruction::StartFunction(name, args, return_type) => {
                    let mut body: Vec<Instruction> = vec![];
                    self.pc += 1;
                    while let Some(instruction) = self.instructions.get(self.pc as usize) {
                        match instruction {
                            Instruction::EndFunction => {
                                break;
                            }
                            Instruction::StartFunction(_, _, _) => {
                                return Err(format!(
                                    "Definition of function inside a function is not allowed."
                                ))
                            }
                            _ => {
                                body.push(instruction.clone());
                                self.pc += 1;
                            }
                        }
                    }
                    if body.len() == 0 {
                        return Err(format!("Function does not return"));
                    }
                    if body.last().unwrap() != &Instruction::Return {
                        body.push(Instruction::Return);
                    }
                    self.functions.insert(
                        name.clone(),
                        (Function::Interpreted(FunctionStruct {
                            name: name.clone(),
                            args: args.to_vec(),
                            body,
                            returns: return_type.clone(),
                        }), false)
                    );
                }
                Instruction::EndFunction => {
                    return Err(format!("EndFunction is not a standalone instruction"))
                }
                Instruction::GCMap => {
                    self.variables.map();
                }
                Instruction::GCTake => {
                    self.variables.take();
                }
                Instruction::ForgetVariable(name) => {
                    self.variables.forget(name.clone());
                }
                Instruction::Dealloc(name) => {
                    self.variables.remove(name);
                }
                Instruction::Duplicate => {
                    if self.stack.len() == 0 {
                        return Err(format!("Cannot use duplicate on an empty stack"))
                    }
                    let val = self.stack.pop().unwrap();
                    self.stack.push(val.clone());
                    self.stack.push(val);
                }
                Instruction::Include(path) => {
                    let newpath = canonicalize(path);
                    if let Err(err) = newpath {
                        return Err(format!("Failed to create include path correctly: {}", err))
                    }
                    let newpath = newpath.unwrap();
                    if self.included.contains(&newpath) {
                        continue;
                    }
                    let res = std::fs::File::open(path);
                    if let Ok(mut file) = res {
                        let instructions = asm::read_instructions(&mut file);
                        if let Err(err) = instructions {
                            return Err(format!("Could not read instructions from file: {}", err))
                        } else if let Ok(ins) = instructions {
                            let mut new_runtime_proto = VirtualMachine::new(ins, path)?;
                            new_runtime_proto.check_labels();
                            new_runtime_proto.run()?;
                            self.functions.extend(new_runtime_proto.functions.into_iter());
                            self.variables.extend(new_runtime_proto.variables.clone().into_iter());
                            self.classes.extend(new_runtime_proto.classes.into_iter());
                            self.included.push(newpath);
                        }
                    } else if let Err(err) = res {
                        return Err(format!("Could not open file '{}': {}", path, err));
                    }
                }
                Instruction::LoopStart => {
                    self.loop_addresses.push(self.pc); // Push the loop start address onto the stack
                }
                Instruction::LoopEnd => {
                    if let Some(Value::Boolean(val)) = self.stack.pop() {
                        if val {
                            if let Some(addr) = self.loop_addresses.last() {
                                self.pc = *addr; // Set the program counter to the loop start address
                                continue;
                            } else {
                                return Err(format!("Return point not set for loop"));
                            }
                        }
                    }
                    self.pc += 1; // Increment the program counter to proceed to the next instruction
                    continue;
                }
                Instruction::InlineAssembly(ins) => {
                    let tokens = tokenize(ins, "<inline-runtime-evaluated>");
                    if let Err(err) = tokens {
                        if self.is_catching {
                            self.stack.push(Value::Error(format!(
                                "Error during evaluation of inline assembly: \n{}",
                                underline_filename_line_column(err)
                            )));
                        } else {
                            return Err(format!(
                                "Error during evaluation of inline assembly: \n{}",
                                underline_filename_line_column(err)
                            ));
                        }
                    } else if let Ok(tokens) = tokens {
                        let mut parser = Parser::new(tokens);
                        let res = parser.parse();
                        if let Ok(instructions) = res {
                            self.instructions.remove(self.pc as usize);
                            let other_part = self
                                .instructions
                                .drain((self.pc as usize)..)
                                .collect::<Vec<Instruction>>();
                            self.instructions
                                .append(&mut VecDeque::from(instructions.clone()));
                            self.instructions.append(&mut VecDeque::from(other_part));
                        } else if let Err(err) = res {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "Error during evaluation of inline assembly: {}",
                                    underline_filename_line_column(err)
                                )));
                            } else {
                                return Err(format!(
                                    "Error during evaluation of inline assembly: {}",
                                    underline_filename_line_column(err)
                                ));
                            }
                        }
                    }
                }
                Instruction::DereferenceRaw => {
                    let item = self.stack.pop();
                    if let Some(item) = item {
                        match item {
                            Value::PtrWrapper(ptr) => {
                                if ptr.is_null() {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "Dereferencing a null pointer is undefined behaviour"
                                        )));
                                    } else {
                                        return Err(format!(
                                            "Dereferencing a null pointer is undefined behaviour"
                                        ));
                                    }
                                } else {
                                    self.stack.push(unsafe { ptr.as_ref().unwrap().clone() });
                                }
                            }
                            _ => {
                                if self.is_catching {
                                    self.stack.push(Value::Error(format!(
                                        "Cannot dereference non-pointer value"
                                    )));
                                } else {
                                    return Err(format!(
                                        "Cannot dereference non-pointer value"
                                    ));
                                }
                            }
                        }
                    } else {
                        if self.is_catching {
                            self.stack.push(Value::Error(format!(
                                "Cannot dereference pointer because the stack is empty"
                            )));
                        } else {
                            return Err(format!(
                                "Cannot dereference pointer because the stack is empty"
                            ));
                        }
                    }
                }
                Instruction::DebuggingPrintStack => {
                    eprintln!("{:?}", self.stack);
                }
                Instruction::MemoryReadVolatile(loc) => {
                    if let Value::PtrWrapper(ptr) = loc {
                        if ptr.is_null() {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "Cannot read from null pointer"
                                )));
                            } else {
                                return Err(format!(
                                    "Cannot read from null pointer"
                                ));
                            }
                        } else {
                            unsafe {
                                let value = std::ptr::read_volatile(*ptr as *const Value);
                                self.stack.push(value);
                            }
                        }
                    } else {
                        if self.is_catching {
                            self.stack.push(Value::Error(format!(
                                "The instruction MemoryReadVolatile expects a pointer as argument"
                            )));
                        } else {
                            return Err(format!(
                                "The instruction MemoryReadVolatile expects a pointer as argument"
                            ));
                        }
                    }
                }
                Instruction::MemoryWriteVolatile(src, dst) => {
                    if let Value::PtrWrapper(ptr) = dst {
                        if ptr.is_null() {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "Cannot write to null pointer"
                                )));
                            } else {
                                return Err(format!(
                                    "Cannot write to null pointer"
                                ));
                            }
                        } else {
                            unsafe {
                                std::ptr::write_volatile(ptr.as_mut().unwrap(), src.clone());
                            }
                        }
                    } else {
                        if self.is_catching {
                            self.stack.push(Value::Error(format!(
                                "The instruction MemoryWriteVolatile expects a pointer as second argument"
                            )));
                        } else {
                            return Err(format!(
                                "The instruction MemoryWriteVolatile expects a pointer as second argument"
                            ));
                        }
                    }
                }
            }
        }
        Ok(Value::None)
    }

    pub fn get_opstack_backtrace(&self, mut limit: usize) -> String {
        let mut trace = String::new();
        trace.push_str("Stack backtrace:\n");

        match std::env::var("Q_STACK_BACKTRACE_LIM") {
            Ok(value) => {
                if value.to_lowercase() == String::from("full") {
                    let remaining_instructions = self.instructions.iter().skip(self.pc as usize);
                    for instr in remaining_instructions {
                        trace.push_str(&format!("\t{}\n", colorize_string(&format!("{:?}", instr))));
                    }
                } else if let Ok(lim) = u64::from_str_radix(&value, 10) {
                    limit = lim as usize;
                    let remaining_instructions = self.instructions.iter().skip(self.pc as usize).take(limit);
                    for instr in remaining_instructions {
                        trace.push_str(&format!("\t{}\n", colorize_string(&format!("{:?}", instr))));
                    }
                } else {
                    if let Err(err) = u64::from_str_radix(&value, 10) {
                        println!("Warning: Invalid enviroment variable Q_STACK_BACKTRACE_LIM value set: can be either a number or full\n\t--> Error parsing number: {}\n\t--> Using default value", err);
                    }
                    let remaining_instructions = self.instructions.iter().skip(self.pc as usize).take(limit);
                    for instr in remaining_instructions {
                        trace.push_str(&format!("\t{}\n", colorize_string(&format!("{:?}", instr))));
                    }
                }
            }
            _ => {
                let remaining_instructions = self.instructions.iter().skip(self.pc as usize).take(limit);
                for instr in remaining_instructions {
                    trace.push_str(&format!("\t{}\n", colorize_string(&format!("{:?}", instr))));
                }
            }
        }

        trace
    }
}