use crate::{
    binary::{asm, asmparser::Parser, asmtokens::tokenize},
    class::Class,
    function::{get_natives, Function, FunctionStruct, FunctionStructNoName},
    memory::allocator::GarbageCollector,
};
use ansi_term::Color;
use ansi_term::Colour::{Blue, Green, White, Yellow};
use fxhash::FxHashMap;
use std::{
    collections::VecDeque,
    fmt::{Debug, Display},
    fs::{canonicalize, File},
    io::{stderr, stdin, stdout, Write, Read},
    os::raw::{c_char, c_double, c_float, c_int, c_long},
    path::PathBuf,
    sync::Arc,
};

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
                    '0'..='9' => {
                        if !is_inside_quotes {
                            Green.paint(c.to_string()).to_string()
                        } else {
                            Blue.paint(c.to_string()).to_string()
                        }
                    },
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
        Color::White.paint(
            bytes
                + parts
                    .into_iter()
                    .skip(3)
                    .collect::<Vec<_>>()
                    .join("")
                    .as_str(),
        )
    } else {
        s.into()
    }
}

pub fn value_to_raw(value: Value) -> RawValue {
    match value {
        Value::None => RawValue::None,
        Value::Int(i) => RawValue::Int(i),
        Value::BigInt(i) => RawValue::BigInt(i.try_into().unwrap()),
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
            return RawValue::RawPtr(
                &value_to_raw(unsafe { p.as_ref().unwrap().clone() }) as *const RawValue
            );
        }
        _ => panic!("Unsupported Value variant"),
    }
}

pub fn raw_to_value(raw_value: RawValue) -> Value {
    match raw_value {
        RawValue::None => Value::None,
        RawValue::Int(i) => Value::Int(i),
        RawValue::BigInt(i) => Value::BigInt(i.into()),
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
        RawValue::RawPtr(p) => panic!("Cannot convert raw pointer to common pointer"),
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
    Function(*const Function),
    FileHandle((*mut File, bool)),
    Byte(u8),
    Bytes(Vec<u8>),
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
    Function,
    FileHandle,
    Byte,
    Bytes,
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
        Value::Function(f) => format!("<function Type at {:?}>", f),
        Value::FileHandle(f) => format!("<file Type at {:?}>", f),
        Value::Byte(b) => format!("{}", b),
        Value::Bytes(b) => format!("{:?}", b),
    }
}

pub fn value_to_typeof(val: &Value) -> String {
    match val {
        Value::None => "None".to_string(),
        Value::Int(_) => "int".to_string(),
        Value::BigInt(_) => "bigint".to_string(),
        Value::Float(_) => "float".to_string(),
        Value::LFloat(_) => "lfloat".to_string(),
        Value::String(_) => "string".to_owned(),
        Value::Character(_) => "char".to_string(),
        Value::List(_) => format!("{}", "array"),
        Value::Tuple(_) => format!("{}", "tuple"),
        Value::Uninitialized => "uninitialized".to_string(),
        Value::Boolean(_) => "boolean".to_string(),
        Value::Error(_) => "error".to_string(),
        Value::Class(_) => format!("class"),
        Value::PtrWrapper(_) => format!("ptr"),
        Value::Function(_) => format!("function"),
        Value::FileHandle(_) => format!("file"),
        Value::Byte(_) => format!("byte"),
        Value::Bytes(_) => format!("bytes"),
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
        }
        Value::Function(f) => {
            format!("<function Type at {:?}>", f)
        }
        Value::FileHandle(f) => {
            format!("<file Type at {:?}>", f)
        }
        Value::Byte(b) => format!("{}", b),
        Value::Bytes(b) => format!("{:?}", b),
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
            (Value::None, (_, Types::None)) => continue,
            (Value::Class(_), (_, Types::Class)) => continue,
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
            (Value::PtrWrapper(_), (_, Types::PtrWrapper)) => {
                continue;
            }
            (Value::Function(_), (_, Types::Function)) => {
                continue;
            }
            (Value::FileHandle(_), (_, Types::FileHandle)) => {
                continue;
            }
            (Value::Byte(_), (_, Types::Byte)) => {
                continue;
            }
            (Value::Bytes(_), (_, Types::Bytes)) => {
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
    Declare(String, Value), // declare variable with name && val
    GetInput,               // variable to store value in
    Print,                  // prints top value to the stdout
    Flush,                  // flushes the stdout
    PrintErr,               // prints to the stderr
    FlushErr,               // flushes the stderr
    Assign(String, Value),  // assigns value to already existing variable
    AssignTop(String),      // assigns value at the top of the stack to already existing variable
    ThrowError(String),     // throws a new error
    Push(Value),            // pushes a value to the stack
    Pop(String),            // pops the top value of the stack to a variable
    PopToRoot(String),
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
    IncludeStd, // includes the std library
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
    Duplicate,
    LoopStart,
    LoopEnd,
    InlineAssembly(String),
    DereferenceRaw,
    DebuggingPrintStack,
    MemoryReadVolatile(Value),
    MemoryWriteVolatile(Value, Value),
    EnterScope,
    LeaveScope,
    Collect,
    AsRef,
    HasRefSameLoc,
    RefDifferenceInLoc,
    Typeof,
    IsInstanceof(String),
    GetFunctionPtr(String),
    InvokeViaPtr,
    PushFunctionAsClosurePtr(Vec<(String, Types)>, Types),
    Cast(Types),
    GetReadFileHandle(String),
    GetWriteFileHandle(String),
    CloseFileHandle(String),
    PushFileHandlePointer(String),
    ReadFromFileHandle(usize),
    ReadFileHandleToString,
    ReadFileHandleToBytes,
    WriteStringToFileHandle,
    WriteBytesToFileHandle,
    SequestrateVariables,
    RestoreSequestratedVariables,
    GetReadFileHandleStack,
    GetWriteFileHandleStack,
    CloseFileHandleStack,
    ReadFromFileHandleStack,
    PushFileHandlePointerStack,

    // Class-related instructions
    DefineClass(String),
    PublicFields,
    DefineField(String, Types),
    EndPublicFields,
    PrivateFields,
    EndPrivateFields,

    LoadFromThisPublic(String),
    LoadFromThisPrivate(String),
    SetThisPublic(String, Value),
    SetThisPrivate(String, Value),
    SetThisStackPublic(String),
    SetThisStackPrivate(String),

    ClassMethodDefinition(String, Vec<(String, Types)>, Types),

    PublicMethods,
    EndPublicMethods,

    PrivateMethods,
    EndPrivateMethods,

    StaticMethods,
    EndStaticMethods,

    InheritFrom(String),

    ConstructorFunctionDefinition(Vec<(String, Types)>, Types),

    // class methods invokation
    Instantiate(String),

    InvokeStaticMethod(String),
    InvokePublicMethod(String),
    InvokePrivateMethod(String),

    SetCurrentObject(String),
    EndClass,

    PushCurrentObject,
    MakeCurrentObjectNone,
}

pub struct VirtualMachine {
    stack: Vec<Value>,
    labels: FxHashMap<String, u32>,
    instructions: VecDeque<Instruction>,
    pc: i64,
    pub variables: FxHashMap<String, *const Value>,
    gc: GarbageCollector,
    pub functions: FxHashMap<String, (Function, bool)>,
    pub classes: FxHashMap<String, Class>,
    pub included: Vec<PathBuf>,
    loop_addresses: Vec<i64>,
    is_catching: bool,
    returns_to: Vec<i64>,
    args_stack: VecDeque<Value>,
    block_len: i64,
    stored_closures: Vec<Function>,
    file_handles_names: VecDeque<String>,
    file_handles: VecDeque<(File, bool)>,
    last_runned: String,
    recursive_level: usize,
    max_recursiveness_level: usize,
    handles_recursion_count: bool,
    sequestrated_variables: Vec<FxHashMap<String, *const Value>>,
    stack_before: Vec<Value>,
    expected_return_types: Vec<Types>,
    current_object: Option<*mut Class>,
}

impl VirtualMachine {
    pub fn new(mut instructions: Vec<Instruction>, filepath: &String) -> Result<Self, String> {
        instructions.push(Instruction::HaltFromStack);
        let mut includeds = Vec::new();
        let res = canonicalize(filepath);
        if let Err(err) = res {
            return Err(format!("{}", err));
        } else if let Ok(included) = res {
            includeds.push(included);
            Ok(Self {
                stack: vec![],
                args_stack: VecDeque::new(),
                labels: FxHashMap::default(),
                instructions: instructions.into(),
                pc: -1,
                loop_addresses: Vec::new(),
                variables: FxHashMap::default(),
                functions: FxHashMap::default(),
                classes: FxHashMap::default(),
                included: includeds,
                is_catching: false,
                returns_to: vec![],
                block_len: 0,
                gc: GarbageCollector::new(),
                stored_closures: Vec::new(),
                file_handles: VecDeque::new(),
                file_handles_names: VecDeque::new(),
                last_runned: String::new(),
                recursive_level: 0,
                max_recursiveness_level: 1000,
                handles_recursion_count: true,
                sequestrated_variables: Vec::new(),
                stack_before: Vec::new(),
                expected_return_types: Vec::new(),
                current_object: None,
            })
        } else {
            unreachable!()
        }
    }

    pub fn set_max_recursiveness_level(&mut self, level: usize) {
        self.max_recursiveness_level = level;
    }

    pub fn set_handles_recursion_count(&mut self, to: bool) {
        self.handles_recursion_count = to;
    }

    pub fn check_labels(&mut self) {
        let mut pos = -1;
        for instruction in self.instructions.iter_mut() {
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

    pub fn link_return(&mut self) {
        let mut i = 0;
        while i < self.instructions.len() {
            if self.instructions[i] == Instruction::Return {
                self.instructions.remove(i);
                self.instructions.insert(i, Instruction::Return);
                self.instructions.insert(i, Instruction::Collect);
                self.instructions.insert(i, Instruction::RestoreSequestratedVariables);
                i += 1;
            }
            i += 1;
        }
    }

    pub fn check_labels_from_pc(&mut self, length: usize) {
        let mut pos = self.pc;
        for instruction in self.instructions.iter().skip(self.pc as usize - 1) {
            pos += 1;
            if pos as usize >= length {
                break;
            }
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

    pub fn allocate_variable(&mut self, name: String, value: Value) {
        let ptr = self.gc.allocate_in_scope(value);
        self.variables.insert(name, ptr.as_ptr());
    }

    pub fn allocate_variable_in_root(&mut self, name: String, value: Value) {
        let ptr = self.gc.allocate(value);
        self.variables.insert(name, ptr.as_ptr());
    }

    pub fn get_variable(&self, name: &str) -> Option<&Value> {
        if let Some(ptr) = self.variables.get(name) {
            Some(unsafe { &**ptr })
        } else {
            None
        }
    }

    pub fn set_variable(&mut self, name: String, value: Value) {
        if let Some(ptr) = self.variables.get_mut(&name) {
            unsafe { *(*ptr as *mut Value) = value };
        } else {
            let ptr = self.gc.allocate_in_scope(value);
            self.variables.insert(name, ptr.as_ptr());
        }
    }

    pub fn collect_garbage(&mut self) {
        self.gc.collect(&mut self.variables);
    }

    pub fn run(&mut self, starts_module_as: String) -> Result<Value, String> {
        self.gc.enter_scope();
        self.allocate_variable_in_root(String::from("__module__"), Value::String(starts_module_as));
        self.allocate_variable_in_root(
            String::from("__function__"),
            Value::String("__main__".to_string()),
        );
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
                    self.allocate_variable(name.clone(), value.clone());
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
                            Value::Class(c) => {
                                let result = write!(stdout(), "{:#?}", c);
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
                        self.set_variable(name.clone(), value.clone());
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
                            self.set_variable(name.clone(), value.clone());
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
                        self.stack.push(unsafe { std::ptr::read_volatile(*value) });
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
                                        self.stack.push(Value::Int(i - i1));
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
                                        self.stack.push(Value::BigInt(i - i1));
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
                                        self.stack.push(Value::Float(f - f1));
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
                                        self.stack.push(Value::LFloat(f - f1));
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
                                        self.stack.push(Value::Int(i / i1));
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
                                        self.stack.push(Value::BigInt(i / i1));
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
                                        self.stack.push(Value::Float(f / f1));
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
                                        self.stack.push(Value::LFloat(f / f1));
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
                                        self.stack.push(Value::Int(i % i1));
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
                                        self.stack.push(Value::BigInt(i % i1));
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
                                        self.stack.push(Value::Float(f % f1));
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
                                        self.stack.push(Value::LFloat(f % f1));
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
                                        self.stack.push(Value::Int(i.pow(i1.try_into().unwrap())));
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
                                            .push(Value::BigInt(i.pow(i1.try_into().unwrap())));
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
                                        self.stack.push(Value::Float(f.powf(f1)));
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
                                        self.stack.push(Value::LFloat(f.powf(f1)));
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
                                        self.stack.push(Value::Boolean(b2 > b1));
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
                                        self.stack.push(Value::Boolean(b2 > b1));
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
                                        self.stack.push(Value::Boolean(b2 > b1));
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
                                        self.stack.push(Value::Boolean(b2 > b1));
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
                                        self.stack.push(Value::Boolean(b2 < b1));
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
                                        self.stack.push(Value::Boolean(b2 < b1));
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
                                        self.stack.push(Value::Boolean(b2 < b1));
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
                                        self.stack.push(Value::Boolean(b2 < b1));
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
                                        self.stack.push(Value::Boolean(b2 >= b1));
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
                                        self.stack.push(Value::Boolean(b2 >= b1));
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
                                        self.stack.push(Value::Boolean(b2 >= b1));
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
                                        self.stack.push(Value::Boolean(b2 >= b1));
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
                                        self.stack.push(Value::Boolean(b2 <= b1));
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
                                        self.stack.push(Value::Boolean(b2 <= b1));
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
                                        self.stack.push(Value::Boolean(b2 <= b1));
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
                                        self.stack.push(Value::Boolean(b2 <= b1));
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
                                if self.handles_recursion_count {
                                    if fun.name == self.last_runned {
                                        self.recursive_level += 1;
                                        if self.recursive_level >= self.max_recursiveness_level {
                                            return Err(format!("Max recursiveness level depth exceeded: Function '{}' called itself {} times.", fun.name, self.recursive_level))
                                        }
                                    } else {
                                        self.recursive_level = 0;
                                        self.last_runned = fun.name.clone();
                                    }
                                }
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
                                let typechecks =
                                    type_check_value(args.clone().into(), fun.args.clone());
                                if let Err(err) = typechecks {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(err));
                                    } else {
                                        return Err(err);
                                    }
                                } else {
                                    self.stack_before = self.stack.clone();
                                    self.expected_return_types.push(fun.returns.clone());
                                    self.returns_to.push(self.pc + 1);
                                    let fun_body_len = fun.body.len();
                                    self.instructions
                                        .drain(self.pc as usize..(self.pc as usize));
                                    let other_part = self
                                        .instructions
                                        .drain((self.pc as usize + 1)..)
                                        .collect::<Vec<Instruction>>();
                                    self.instructions
                                        .append(&mut VecDeque::from(fun.body.clone()));
                                    self.instructions.append(&mut VecDeque::from(other_part));
                                    self.block_len = fun_body_len as i64;
                                    let zipped = fun.args.clone().into_iter();
                                    let zipped = zipped.zip(args.into_iter());
                                    for ((varname, _), value) in zipped {
                                        self.allocate_variable(varname, value);
                                    }
                                    self.check_labels_from_pc(fun_body_len);
                                }
                            }
                            _ => {
                                return Err(format!("How did we get here? Closures cannot be stored as normal functions!"));
                            }
                        }
                    }
                }
                Instruction::Pop(name) => {
                    let top = self.stack.pop();
                    if let Some(top) = top {
                        self.allocate_variable(name.clone(), top);
                    } else if self.is_catching {
                        self.stack
                            .push(Value::Error(String::from("The stack is empty")));
                    } else {
                        return Err(String::from("The stack is empty"));
                    }
                }
                Instruction::PopToRoot(name) => {
                    let top = self.stack.pop();
                    if let Some(top) = top {
                        self.allocate_variable_in_root(name.clone(), top);
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
                            let value_to_return = self.stack.pop();
                            match value_to_return {
                                Some(value) => {
                                    if self.stack_before != self.stack {
                                        return Err(format!("Type checking failed: the function `{}` (latest function called) modified the stack beyond its bounds", self.last_runned))
                                    } else {
                                        let expected_to_return = self.expected_return_types.pop();
                                        match expected_to_return {
                                            Some(type_) => {
                                                let typechecks = type_check_value(vec![value.clone()], vec![(String::from("random_return_arg_placeholder"), type_.clone())]);
                                                match typechecks {
                                                    Ok(_) => {
                                                        self.stack.push(value);
                                                        continue;
                                                    }
                                                    Err(_) => {
                                                        return Err(format!("Type checking failed: the function `{}` has a signature with return type `{}` but returned a value of type `{}` ({:?}).\nMaybe change the return type to `{}`?",
                                                        self.last_runned, format!("{:?}", type_).to_lowercase(), value_to_typeof(&value), &value, value_to_typeof(&value)))
                                                    }
                                                }
                                            }
                                            None => {
                                                return Err(format!("Type checking failed: the function {} wasn't expected to return from this point", self.last_runned))
                                            }
                                        }
                                    }
                                }
                                None => {

                                }
                            }
                        }
                        None => {
                            self.pc = (self.instructions.len() - 1) as i64;
                        }
                    }
                }
                Instruction::GetClassProperty(classname, property) => {
                    panic!("GetClassProperty is deprecated")
                }
                Instruction::InvokeClassMethod(classname, name) => {
                    panic!("InvokeClassMethod is deprecated")
                }
                Instruction::SetClassProperty(classname, name, val) => {
                    panic!("SetClassProperty is deprecated")
                }
                Instruction::ClassHasProperty(classname, property) => {
                    panic!("ClassHasProperty is deprecated")
                }
                Instruction::ClassHasStaticMethod(classname, staticmethod) => {
                    panic!("ClassHasStaticMethod is deprecated")
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
                    body.push(Instruction::SequestrateVariables);
                    body.push(Instruction::Push(Value::String(name.clone())));
                    body.push(Instruction::PopToRoot(String::from("__function__")));
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
                    if body.len() == 4 {
                        return Err(format!("Function does not return"));
                    }
                    if body.last().unwrap() != &Instruction::Return {
                        body.push(Instruction::Return);
                    }
                    self.functions.insert(
                        name.clone(),
                        (
                            Function::Interpreted(FunctionStruct {
                                name: name.clone(),
                                args: args.to_vec(),
                                body,
                                returns: return_type.clone(),
                            }),
                            false,
                        ),
                    );
                }
                Instruction::EndFunction => {
                    return Err(format!("EndFunction is not a standalone instruction"))
                }
                Instruction::Duplicate => {
                    if self.stack.len() == 0 {
                        return Err(format!("Cannot use duplicate on an empty stack"));
                    }
                    let val = self.stack.pop().unwrap();
                    self.stack.push(val.clone());
                    self.stack.push(val);
                }
                Instruction::Include(path) => {
                    let newpath = canonicalize(path);
                    if let Err(err) = newpath {
                        return Err(format!("Failed to create include path correctly: {}", err));
                    }
                    let newpath = newpath.unwrap();
                    if self.included.contains(&newpath) {
                        continue;
                    }
                    let res = std::fs::File::open(path);
                    if let Ok(mut file) = res {
                        let instructions = asm::read_instructions(&mut file);
                        if let Err(err) = instructions {
                            return Err(format!("Could not read instructions from file: {err}"));
                        } else if let Ok(ins) = instructions {
                            let mut new_runtime_proto = VirtualMachine::new(ins, path)?;
                            new_runtime_proto.check_labels();
                            new_runtime_proto.run(String::from("__module__"))?;
                            self.functions
                                .extend(new_runtime_proto.functions.into_iter());
                            self.gc.absorb(&mut new_runtime_proto.gc);
                            self.stored_closures
                                .append(&mut new_runtime_proto.stored_closures);
                            self.variables
                                .extend(new_runtime_proto.variables.clone().into_iter());
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
                                    return Err(format!("Cannot dereference non-pointer value"));
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
                                self.stack
                                    .push(Value::Error(format!("Cannot read from null pointer")));
                            } else {
                                return Err(format!("Cannot read from null pointer"));
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
                                self.stack
                                    .push(Value::Error(format!("Cannot write to null pointer")));
                            } else {
                                return Err(format!("Cannot write to null pointer"));
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
                Instruction::Collect => {
                    self.collect_garbage();
                }
                Instruction::EnterScope => {
                    self.gc.enter_scope();
                }
                Instruction::LeaveScope => {
                    self.gc.leave_scope();
                }
                Instruction::AsRef => {
                    if let Some(mut val) = self.stack.pop() {
                        self.stack.push(Value::PtrWrapper(&mut val as *mut Value));
                        self.stack.push(val);
                        let len = self.stack.len();
                        self.stack.swap(len - 1, len - 2);
                    } else {
                        if self.is_catching {
                            self.stack.push(Value::Error(
                                "Could not convert to pointer: The stack is empty".to_string(),
                            ))
                        } else {
                            return Err(
                                "Could not convert to pointer: The stack is empty".to_string()
                            );
                        }
                    }
                }
                Instruction::HasRefSameLoc => {
                    if let Some(ptr) = self.stack.pop() {
                        if let Some(ptr2) = self.stack.pop() {
                            if let Value::PtrWrapper(ptr) = ptr {
                                if let Value::PtrWrapper(ptr2) = ptr2 {
                                    self.stack.push(Value::Boolean(ptr == ptr2));
                                } else {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(
                                            "HasRefSameLoc expects two pointers for arguments"
                                                .to_string(),
                                        ))
                                    } else {
                                        return Err(
                                            "HasRefSameLoc expects two pointers for arguments"
                                                .to_string(),
                                        );
                                    }
                                }
                            } else {
                                if self.is_catching {
                                    self.stack.push(Value::Error(
                                        "HasRefSameLoc expects two pointers for arguments"
                                            .to_string(),
                                    ))
                                } else {
                                    return Err("HasRefSameLoc expects two pointers for arguments"
                                        .to_string());
                                }
                            }
                        } else {
                            if self.is_catching {
                                self.stack.push(Value::Error(
                                    "Could not compare pointer's location: The stack has only one operand".to_string(),
                                ))
                            } else {
                                return Err(
                                    "Could not compare pointer's location: The stack has only one operand".to_string()
                                );
                            }
                        }
                    } else {
                        if self.is_catching {
                            self.stack.push(Value::Error(
                                "Could not compare pointer's location: The stack is empty"
                                    .to_string(),
                            ))
                        } else {
                            return Err("Could not compare pointer's location: The stack is empty"
                                .to_string());
                        }
                    }
                }
                Instruction::RefDifferenceInLoc => {
                    if let Some(ptr) = self.stack.pop() {
                        if let Some(ptr2) = self.stack.pop() {
                            if let Value::PtrWrapper(ptr) = ptr {
                                if let Value::PtrWrapper(ptr2) = ptr2 {
                                    self.stack.push(Value::BigInt(unsafe {
                                        (ptr as *const u8).offset_from(ptr2 as *const u8)
                                    }
                                        as i64));
                                } else {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(
                                            "HasRefSameLoc expects two pointers for arguments"
                                                .to_string(),
                                        ))
                                    } else {
                                        return Err(
                                            "HasRefSameLoc expects two pointers for arguments"
                                                .to_string(),
                                        );
                                    }
                                }
                            } else {
                                if self.is_catching {
                                    self.stack.push(Value::Error(
                                        "HasRefSameLoc expects two pointers for arguments"
                                            .to_string(),
                                    ))
                                } else {
                                    return Err("HasRefSameLoc expects two pointers for arguments"
                                        .to_string());
                                }
                            }
                        } else {
                            if self.is_catching {
                                self.stack.push(Value::Error(
                                    "Could not compare pointer's location: The stack has only one operand".to_string(),
                                ))
                            } else {
                                return Err(
                                    "Could not compare pointer's location: The stack has only one operand".to_string()
                                );
                            }
                        }
                    } else {
                        if self.is_catching {
                            self.stack.push(Value::Error(
                                "Could not compare pointer's location: The stack is empty"
                                    .to_string(),
                            ))
                        } else {
                            return Err("Could not compare pointer's location: The stack is empty"
                                .to_string());
                        }
                    }
                }
                Instruction::Typeof => {
                    if let Some(val) = self.stack.pop() {
                        self.stack.push(Value::String(value_to_typeof(&val)));
                        self.stack.push(val);
                        let len = self.stack.len();
                        self.stack.swap(len - 1, len - 2);
                    } else {
                        if self.is_catching {
                            self.stack.push(Value::Error(
                                "Couldn't apply typeof operator: The stack is empty".to_string(),
                            ))
                        } else {
                            return Err(
                                "Couldn't apply typeof operator: The stack is empty".to_string()
                            );
                        }
                    }
                }
                Instruction::IsInstanceof(name) => {
                    if let Some(val) = self.stack.pop() {
                        if let Value::Class(c) = val {
                            if &c.name == name {
                                self.stack.push(Value::Class(c));
                                self.stack.push(Value::Boolean(true));
                            } else {
                                self.stack.push(Value::Class(c));
                                self.stack.push(Value::Boolean(false));
                            }
                        } else {
                            self.stack.push(val);
                            self.stack.push(Value::Boolean(false));
                        }
                    } else {
                        if self.is_catching {
                            self.stack.push(Value::Error(
                                "Couldn't apply isinstanceof operator: The stack is empty"
                                    .to_string(),
                            ))
                        } else {
                            return Err("Couldn't apply isinstanceof operator: The stack is empty"
                                .to_string());
                        }
                    }
                }
                Instruction::GetFunctionPtr(name) => {
                    if let Some((function, _)) = self.functions.get(name) {
                        self.stack
                            .push(Value::Function(function as *const Function));
                    } else {
                        if self.is_catching {
                            self.stack.push(Value::Error(
                                "Couldn't push function pointer to stack: Function not found"
                                    .to_string(),
                            ))
                        } else {
                            return Err(
                                "Couldn't push function pointer to stack: Function not found"
                                    .to_string(),
                            );
                        }
                    }
                }
                Instruction::InvokeViaPtr => {
                    if let Some(f) = self.stack.pop() {
                        if let Value::Function(f) = f {
                            let function = unsafe { std::ptr::read_volatile(f) };
                            match function {
                                Function::Native(fun) => {
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
                                Function::Interpreted(fun) => {
                                    if self.handles_recursion_count {
                                        if fun.name == self.last_runned {
                                            self.recursive_level += 1;
                                            if self.recursive_level >= self.max_recursiveness_level {
                                                return Err(format!("Max recursiveness level depth exceeded: Function '{}' called itself {} times.", fun.name, self.recursive_level))
                                            }
                                        } else {
                                            self.recursive_level = 0;
                                        }
                                    }
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
                                    let typechecks =
                                        type_check_value(args.clone().into(), fun.args.clone());
                                    if let Err(err) = typechecks {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(err));
                                        } else {
                                            return Err(err);
                                        }
                                    } else {
                                        self.stack_before = self.stack.clone();
                                        self.expected_return_types.push(fun.returns.clone());
                                        self.returns_to.push(self.pc + 1); // Dunno why 3, if i put any other num here it crashes
                                        let fun_body_len = fun.body.len();
                                        self.instructions
                                            .drain(self.pc as usize..(self.pc as usize));
                                        let other_part = self
                                            .instructions
                                            .drain((self.pc as usize)..)
                                            .collect::<Vec<Instruction>>();
                                        self.instructions
                                            .append(&mut VecDeque::from(fun.body.clone()));
                                        self.instructions.append(&mut VecDeque::from(other_part));
                                        self.block_len = fun_body_len as i64;
                                        let zipped = fun.args.clone().into_iter();
                                        let zipped = zipped.zip(args.into_iter());
                                        for ((varname, _), value) in zipped {
                                            self.allocate_variable(varname, value);
                                        }
                                    }
                                }
                                Function::Closure(fun) => {
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
                                    let typechecks =
                                        type_check_value(args.clone().into(), fun.args.clone());
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
                                        self.instructions
                                            .append(&mut VecDeque::from(fun.body.clone()));
                                        self.instructions.append(&mut VecDeque::from(other_part));
                                        self.block_len = fun_body_len as i64;
                                        let zipped = fun.args.clone().into_iter();
                                        let zipped = zipped.zip(args.into_iter());
                                        for ((varname, _), value) in zipped {
                                            self.allocate_variable(varname, value);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                Instruction::PushFunctionAsClosurePtr(args, return_type) => {
                    let mut body: Vec<Instruction> = vec![];
                    body.push(Instruction::Push(Value::String(String::from("closure"))));
                    body.push(Instruction::PopToRoot(String::from("__function__")));
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
                    if body.len() == 2 {
                        return Err(format!("Function does not return"));
                    }
                    if body.last().unwrap() != &Instruction::Return {
                        body.push(Instruction::Return);
                    }
                    self.stored_closures
                        .push(Function::Closure(FunctionStructNoName {
                            args: args.to_vec(),
                            body,
                            returns: return_type.clone(),
                        }));
                    let reference = self
                        .stored_closures
                        .get(self.stored_closures.len() - 1)
                        .unwrap();
                    self.stack
                        .push(Value::Function(reference as *const Function));
                }
                Instruction::Cast(t) => match t {
                    Types::None
                    | Types::Class
                    | Types::Uninitialized
                    | Types::PtrWrapper
                    | Types::Any => {
                        if self.is_catching {
                            self.stack
                                .push(Value::Error(format!("Cannot cast value into {:?}", t)));
                        } else {
                            return Err(format!("Cannot cast value into {:?}", t));
                        }
                    }
                    Types::Int | Types::BigInt | Types::Float | Types::LFloat => {
                        if let Some(val) = self.stack.pop() {
                            match val {
                                Value::Int(i) => match t {
                                    Types::Int => {
                                        self.stack.push(Value::Int(i));
                                    }
                                    Types::BigInt => {
                                        self.stack.push(Value::BigInt(i64::from(i)));
                                    }
                                    Types::Float => {
                                        self.stack.push(Value::Float(i as f64));
                                    }
                                    Types::LFloat => {
                                        self.stack.push(Value::LFloat(i as f32));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!(
                                                "Cannot cast value into {:?}: Invalid target type",
                                                t
                                            )));
                                        } else {
                                            return Err(format!(
                                                "Cannot cast value into {:?}: Invalid target type",
                                                t
                                            ));
                                        }
                                    }
                                },
                                Value::BigInt(i) => match t {
                                    Types::Int => {
                                        if let Some(casted) = i.try_into().ok() {
                                            self.stack.push(Value::Int(casted));
                                        } else {
                                            if self.is_catching {
                                                self.stack.push(Value::Error(format!(
                                                            "Cannot cast value into {:?}: Overflow or underflow occurred",
                                                            t
                                                        )));
                                            } else {
                                                return Err(format!(
                                                            "Cannot cast value into {:?}: Overflow or underflow occurred",
                                                            t
                                                        ));
                                            }
                                        }
                                    }
                                    Types::BigInt => {
                                        self.stack.push(Value::BigInt(i));
                                    }
                                    Types::Float => {
                                        self.stack.push(Value::Float(i as f64));
                                    }
                                    Types::LFloat => {
                                        self.stack.push(Value::LFloat(i as f32));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!(
                                                "Cannot cast value into {:?}: Invalid target type",
                                                t
                                            )));
                                        } else {
                                            return Err(format!(
                                                "Cannot cast value into {:?}: Invalid target type",
                                                t
                                            ));
                                        }
                                    }
                                },
                                Value::Float(f) => match t {
                                    Types::Int => {
                                        if let Ok(casted) = i32::try_from(f as i64) {
                                            self.stack.push(Value::Int(casted));
                                        } else {
                                            if self.is_catching {
                                                self.stack.push(Value::Error(format!(
                                                            "Cannot cast value into {:?}: Overflow or underflow occurred",
                                                            t
                                                        )));
                                            } else {
                                                return Err(format!(
                                                            "Cannot cast value into {:?}: Overflow or underflow occurred",
                                                            t
                                                        ));
                                            }
                                        }
                                    }
                                    Types::BigInt => {
                                        if let casted = f as i64 {
                                            self.stack.push(Value::BigInt(casted));
                                        } else {
                                            if self.is_catching {
                                                self.stack.push(Value::Error(format!(
                                                            "Cannot cast value into {:?}: Overflow or underflow occurred",
                                                            t
                                                        )));
                                            } else {
                                                return Err(format!(
                                                            "Cannot cast value into {:?}: Overflow or underflow occurred",
                                                            t
                                                        ));
                                            }
                                        }
                                    }
                                    Types::Float => {
                                        self.stack.push(Value::Float(f));
                                    }
                                    Types::LFloat => {
                                        if let casted = f as f32 {
                                            self.stack.push(Value::LFloat(casted));
                                        } else {
                                            if self.is_catching {
                                                self.stack.push(Value::Error(format!(
                                                            "Cannot cast value into {:?}: Overflow or underflow occurred",
                                                            t
                                                        )));
                                            } else {
                                                return Err(format!(
                                                            "Cannot cast value into {:?}: Overflow or underflow occurred",
                                                            t
                                                        ));
                                            }
                                        }
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!(
                                                "Cannot cast value into {:?}: Invalid target type",
                                                t
                                            )));
                                        } else {
                                            return Err(format!(
                                                "Cannot cast value into {:?}: Invalid target type",
                                                t
                                            ));
                                        }
                                    }
                                },
                                Value::LFloat(f) => match t {
                                    Types::Int => {
                                        if let Ok(casted) = i32::try_from(f as i64) {
                                            self.stack.push(Value::Int(casted));
                                        } else {
                                            if self.is_catching {
                                                self.stack.push(Value::Error(format!(
                                                            "Cannot cast value into {:?}: Overflow or underflow occurred",
                                                            t
                                                        )));
                                            } else {
                                                return Err(format!(
                                                            "Cannot cast value into {:?}: Overflow or underflow occurred",
                                                            t
                                                        ));
                                            }
                                        }
                                    }
                                    Types::BigInt => {
                                        if let casted = f as i64 {
                                            self.stack.push(Value::BigInt(casted));
                                        } else {
                                            if self.is_catching {
                                                self.stack.push(Value::Error(format!(
                                                            "Cannot cast value into {:?}: Overflow or underflow occurred",
                                                            t
                                                        )));
                                            } else {
                                                return Err(format!(
                                                            "Cannot cast value into {:?}: Overflow or underflow occurred",
                                                            t
                                                        ));
                                            }
                                        }
                                    }
                                    Types::Float => {
                                        self.stack.push(Value::Float(f as f64));
                                    }
                                    Types::LFloat => {
                                        self.stack.push(Value::LFloat(f));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!(
                                                "Cannot cast value into {:?}: Invalid target type",
                                                t
                                            )));
                                        } else {
                                            return Err(format!(
                                                "Cannot cast value into {:?}: Invalid target type",
                                                t
                                            ));
                                        }
                                    }
                                },
                                Value::Boolean(b) => match t {
                                    Types::Int => {
                                        let i = if b { 1 } else { 0 };
                                        self.stack.push(Value::Int(i));
                                    }
                                    Types::BigInt => {
                                        let i = if b { 1 } else { 0 };
                                        self.stack.push(Value::BigInt(i.into()));
                                    }
                                    Types::Float => {
                                        let f = if b { 1.0 } else { 0.0 };
                                        self.stack.push(Value::Float(f));
                                    }
                                    Types::LFloat => {
                                        let f = if b { 1.0 } else { 0.0 };
                                        self.stack.push(Value::LFloat(f as f32));
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!(
                                                "Cannot cast value into {:?}: Invalid target type",
                                                t
                                            )));
                                        } else {
                                            return Err(format!(
                                                "Cannot cast value into {:?}: Invalid target type",
                                                t
                                            ));
                                        }
                                    }
                                },
                                Value::String(s) => match t {
                                    Types::Int => {
                                        if let Ok(parsed) = s.parse::<i32>() {
                                            self.stack.push(Value::Int(parsed));
                                        } else {
                                            if self.is_catching {
                                                self.stack.push(Value::Error(format!(
                                                            "Cannot cast value into {:?}: Failed to parse string as integer",
                                                            t
                                                        )));
                                            } else {
                                                return Err(format!(
                                                            "Cannot cast value into {:?}: Failed to parse string as integer",
                                                            t
                                                        ));
                                            }
                                        }
                                    }
                                    Types::BigInt => {
                                        if let Ok(parsed) = s.parse::<i64>() {
                                            self.stack.push(Value::BigInt(parsed));
                                        } else {
                                            if self.is_catching {
                                                self.stack.push(Value::Error(format!(
                                                            "Cannot cast value into {:?}: Failed to parse string as big integer",
                                                            t
                                                        )));
                                            } else {
                                                return Err(format!(
                                                            "Cannot cast value into {:?}: Failed to parse string as big integer",
                                                            t
                                                        ));
                                            }
                                        }
                                    }
                                    Types::Float => {
                                        if let Ok(parsed) = s.parse::<f64>() {
                                            self.stack.push(Value::Float(parsed));
                                        } else {
                                            if self.is_catching {
                                                self.stack.push(Value::Error(format!(
                                                            "Cannot cast value into {:?}: Failed to parse string as float",
                                                            t
                                                        )));
                                            } else {
                                                return Err(format!(
                                                            "Cannot cast value into {:?}: Failed to parse string as float",
                                                            t
                                                        ));
                                            }
                                        }
                                    }
                                    Types::LFloat => {
                                        if let Ok(parsed) = s.parse::<f32>() {
                                            self.stack.push(Value::LFloat(parsed));
                                        } else {
                                            if self.is_catching {
                                                self.stack.push(Value::Error(format!(
                                                            "Cannot cast value into {:?}: Failed to parse string as long float",
                                                            t
                                                        )));
                                            } else {
                                                return Err(format!(
                                                            "Cannot cast value into {:?}: Failed to parse string as long float",
                                                            t
                                                        ));
                                            }
                                        }
                                    }
                                    _ => {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!(
                                                "Cannot cast value into {:?}: Invalid target type",
                                                t
                                            )));
                                        } else {
                                            return Err(format!(
                                                "Cannot cast value into {:?}: Invalid target type",
                                                t
                                            ));
                                        }
                                    }
                                },
                                Value::Character(c) => {
                                    let s = c.to_string();
                                    match t {
                                        Types::String => {
                                            self.stack.push(Value::String(s));
                                        }
                                        Types::Int => {
                                            if let Some(parsed) = s.parse::<i32>().ok() {
                                                self.stack.push(Value::Int(parsed));
                                            } else {
                                                if self.is_catching {
                                                    self.stack.push(Value::Error(format!(
                                                            "Cannot cast value into {:?}: Failed to parse character as integer",
                                                            t
                                                        )));
                                                } else {
                                                    return Err(format!(
                                                            "Cannot cast value into {:?}: Failed to parse character as integer",
                                                            t
                                                        ));
                                                }
                                            }
                                        }
                                        Types::BigInt => {
                                            if let Some(parsed) = s.parse::<i64>().ok() {
                                                self.stack.push(Value::BigInt(parsed));
                                            } else {
                                                if self.is_catching {
                                                    self.stack.push(Value::Error(format!(
                                                            "Cannot cast value into {:?}: Failed to parse character as big integer",
                                                            t
                                                        )));
                                                } else {
                                                    return Err(format!(
                                                            "Cannot cast value into {:?}: Failed to parse character as big integer",
                                                            t
                                                        ));
                                                }
                                            }
                                        }
                                        Types::Float => {
                                            if let Some(parsed) = s.parse::<f64>().ok() {
                                                self.stack.push(Value::Float(parsed));
                                            } else {
                                                if self.is_catching {
                                                    self.stack.push(Value::Error(format!(
                                                            "Cannot cast value into {:?}: Failed to parse character as float",
                                                            t
                                                        )));
                                                } else {
                                                    return Err(format!(
                                                            "Cannot cast value into {:?}: Failed to parse character as float",
                                                            t
                                                        ));
                                                }
                                            }
                                        }
                                        Types::LFloat => {
                                            if let Some(parsed) = s.parse::<f32>().ok() {
                                                self.stack.push(Value::LFloat(parsed));
                                            } else {
                                                if self.is_catching {
                                                    self.stack.push(Value::Error(format!(
                                                            "Cannot cast value into {:?}: Failed to parse character as long float",
                                                            t
                                                        )));
                                                } else {
                                                    return Err(format!(
                                                            "Cannot cast value into {:?}: Failed to parse character as long float",
                                                            t
                                                        ));
                                                }
                                            }
                                        }
                                        _ => {
                                            if self.is_catching {
                                                self.stack.push(Value::Error(format!(
                                                        "Cannot cast value into {:?}: Invalid target type",
                                                        t
                                                    )));
                                            } else {
                                                return Err(format!(
                                                        "Cannot cast value into {:?}: Invalid target type",
                                                        t
                                                    ));
                                            }
                                        }
                                    }
                                }
                                _ => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "Cannot cast value into {:?}: Invalid source type",
                                            val
                                        )));
                                    } else {
                                        return Err(format!(
                                            "Cannot cast value into {:?}: Invalid source type",
                                            val
                                        ));
                                    }
                                }
                            }
                        } else {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "Cannot cast value into {:?}: The stack is empty",
                                    t
                                )));
                            } else {
                                return Err(format!(
                                    "Cannot cast value into {:?}: The stack is empty",
                                    t
                                ));
                            }
                        }
                    }
                    _ => {
                        if self.is_catching {
                            self.stack.push(Value::Error(format!(
                                "Cannot cast value into {:?}: Invalid target type",
                                t
                            )));
                        } else {
                            return Err(format!(
                                "Cannot cast value into {:?}: Invalid target type",
                                t
                            ));
                        }
                    }
                }
                Instruction::GetReadFileHandle(name) => {
                    let res = File::open(name);
                    match res {
                        Ok(file) => {
                            self.file_handles.push_back((file, false));
                            self.file_handles_names.push_back(name.clone());
                            let len = self.file_handles.len();
                            let file_pointer = &mut self.file_handles.get_mut(len - 1).unwrap().0 as *mut File;
                            self.stack.push(Value::FileHandle((file_pointer, false)));
                        }
                        Err(err) => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "Error creating read file handle: {}", err
                                )));
                            } else {
                                return Err(format!(
                                    "Error creating read file handle: {}", err
                                ));
                            }
                        }
                    }
                }
                Instruction::GetWriteFileHandle(name) => {
                    let res = File::options().read(true).write(true).create(true).open(name);
                    match res {
                        Ok(file) => {
                            self.file_handles.push_back((file, true));
                            self.file_handles_names.push_back(name.clone());
                            let len = self.file_handles.len();
                            let file_pointer = &mut self.file_handles.get_mut(len - 1).unwrap().0 as *mut File;
                            self.stack.push(Value::FileHandle((file_pointer, true)));
                        }
                        Err(err) => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "Error creating write file handle: {}", err
                                )));
                            } else {
                                return Err(format!(
                                    "Error creating write file handle: {}", err
                                ));
                            }
                        }
                    }
                }
                Instruction::CloseFileHandle(name) => {
                    let indexof = self.file_handles_names.binary_search(name);
                    match indexof {
                        Ok(index) => {
                            let _handle = self.file_handles.remove(index).unwrap();
                            self.file_handles_names.remove(index);
                        }
                        Err(_) => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "The file handle for '{}' is not opened.", name
                                )));
                            } else {
                                return Err(format!(
                                    "The file handle for '{}' is not opened.", name
                                ));
                            }
                        }
                    }
                }
                Instruction::PushFileHandlePointer(name) => {
                    let indexof = self.file_handles_names.binary_search(name);
                    match indexof {
                        Ok(index) => {
                            let (handle, is_writeable) = self.file_handles.get_mut(index).unwrap();
                            self.stack.push(Value::FileHandle((handle as *mut File, *is_writeable)));
                        }
                        Err(_) => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "The file handle for '{}' is not opened.", name
                                )));
                            } else {
                                return Err(format!(
                                    "The file handle for '{}' is not opened.", name
                                ));
                            }
                        }
                    }
                }
                Instruction::ReadFromFileHandle(bytes) => {
                    match self.stack.pop() {
                        Some(Value::FileHandle((handle, _))) => {
                            let mut reference = unsafe { handle.as_mut().unwrap() };
                            let mut buffer = Vec::with_capacity(*bytes);
                            let res = reference.read_exact(&mut buffer);
                            match res {
                                Ok(_) => {
                                    self.stack.push(Value::Bytes(buffer));
                                }
                                Err(e) => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "Error reading {} bytes from file handle: {}", bytes, e
                                        )));
                                    } else {
                                        return Err(format!(
                                            "Error reading {} bytes from file handle: {}", bytes, e
                                        ));
                                    }
                                }
                            }
                        }
                        Some(_) => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "Tried to read from a non-File handle object"
                                )));
                            } else {
                                return Err(format!(
                                    "Tried to read from a non-File handle object"
                                ));
                            }
                        }
                        None => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "The stack is empty"
                                )));
                            } else {
                                return Err(format!(
                                    "The stack is empty"
                                ));
                            }
                        }
                    }
                }
                Instruction::ReadFileHandleToString => {
                    match self.stack.pop() {
                        Some(Value::FileHandle((handle, _))) => {
                            let mut reference = unsafe { handle.as_mut().unwrap() };
                            let mut buffer = String::new();
                            let res = reference.read_to_string(&mut buffer);
                            match res {
                                Ok(_) => {
                                    self.stack.push(Value::String(buffer));
                                }
                                Err(e) => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "Error reading file handle to string: {}", e
                                        )));
                                    } else {
                                        return Err(format!(
                                            "Error reading file handle to string: {}", e
                                        ));
                                    }
                                }
                            }
                        }
                        Some(_) => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "Tried to read from a non-File handle object"
                                )));
                            } else {
                                return Err(format!(
                                    "Tried to read from a non-File handle object"
                                ));
                            }
                        }
                        None => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "The stack is empty"
                                )));
                            } else {
                                return Err(format!(
                                    "The stack is empty"
                                ));
                            }
                        }
                    }
                }
                Instruction::ReadFileHandleToBytes => {
                    match self.stack.pop() {
                        Some(Value::FileHandle((handle, _))) => {
                            let mut reference = unsafe { handle.as_mut().unwrap() };
                            let mut buffer = Vec::new();
                            let res = reference.read_to_end(&mut buffer);
                            match res {
                                Ok(_) => {
                                    self.stack.push(Value::Bytes(buffer));
                                }
                                Err(e) => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "Error reading file handle to bytes: {}", e
                                        )));
                                    } else {
                                        return Err(format!(
                                            "Error reading file handle to bytes: {}", e
                                        ));
                                    }
                                }
                            }
                        }
                        Some(_) => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "Tried to read from a non-File handle object"
                                )));
                            } else {
                                return Err(format!(
                                    "Tried to read from a non-File handle object"
                                ));
                            }
                        }
                        None => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "The stack is empty"
                                )));
                            } else {
                                return Err(format!(
                                    "The stack is empty"
                                ));
                            }
                        }
                    }
                }
                Instruction::WriteStringToFileHandle => {
                    let string = self.stack.pop();
                    let file_ref = self.stack.pop();
                    match string {
                        Some(Value::String(s)) => {
                            match file_ref {
                                Some(Value::FileHandle((handle, is_writeable))) => {
                                    if !is_writeable {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!(
                                                "The specified file handle is not writeable"
                                            )));
                                        } else {
                                            return Err(format!(
                                                "The specified file handle is not writeable"
                                            ));
                                        }
                                    } else {
                                        let reference = unsafe { handle.as_mut().unwrap() };
                                        let res = write!(reference, "{}", s);
                                        match res {
                                            Err(err) => {
                                                if self.is_catching {
                                                    self.stack.push(Value::Error(format!(
                                                        "Error writing to file handle: {}", err
                                                    )));
                                                } else {
                                                    return Err(format!(
                                                        "Error writing to file handle: {}", err
                                                    ));
                                                }
                                            }
                                            _ => continue,
                                        }
                                    }
                                }
                                Some(_) => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "Expected a file handle to write to"
                                        )));
                                    } else {
                                        return Err(format!(
                                            "Expected a file handle to write to"
                                        ));
                                    }
                                }
                                None => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "The stack is empty"
                                        )));
                                    } else {
                                        return Err(format!(
                                            "The stack is empty"
                                        ));
                                    }
                                }
                            }
                        }
                        Some(_) => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "Expected string to write to a file handle"
                                )));
                            } else {
                                return Err(format!(
                                    "Expected string to write to a file handle"
                                ));
                            }
                        }
                        None => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "The stack is empty"
                                )));
                            } else {
                                return Err(format!(
                                    "The stack is empty"
                                ));
                            }
                        }
                    }
                }
                Instruction::WriteBytesToFileHandle => {
                    let bytes = self.stack.pop();
                    let file_ref = self.stack.pop();
                    match bytes {
                        Some(Value::Bytes(b)) => {
                            match file_ref {
                                Some(Value::FileHandle((handle, is_writeable))) => {
                                    if !is_writeable {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!(
                                                "The specified file handle is not writeable"
                                            )));
                                        } else {
                                            return Err(format!(
                                                "The specified file handle is not writeable"
                                            ));
                                        }
                                    } else {
                                        let reference = unsafe { handle.as_mut().unwrap() };
                                        let res = reference.write_all(b.as_slice());
                                        match res {
                                            Err(err) => {
                                                if self.is_catching {
                                                    self.stack.push(Value::Error(format!(
                                                        "Error writing to file handle: {}", err
                                                    )));
                                                } else {
                                                    return Err(format!(
                                                        "Error writing to file handle: {}", err
                                                    ));
                                                }
                                            }
                                            _ => continue,
                                        }
                                    }
                                }
                                Some(_) => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "Expected a file handle to write to"
                                        )));
                                    } else {
                                        return Err(format!(
                                            "Expected a file handle to write to"
                                        ));
                                    }
                                }
                                None => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "The stack is empty"
                                        )));
                                    } else {
                                        return Err(format!(
                                            "The stack is empty"
                                        ));
                                    }
                                }
                            }
                        }
                        Some(_) => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "Expected bytes to write to a file handle"
                                )));
                            } else {
                                return Err(format!(
                                    "Expected bytes to write to a file handle"
                                ));
                            }
                        }
                        None => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "The stack is empty"
                                )));
                            } else {
                                return Err(format!(
                                    "The stack is empty"
                                ));
                            }
                        }
                    }
                }
                Instruction::RestoreSequestratedVariables => {
                    if let Some(v) = self.sequestrated_variables.pop() {
                        self.variables = v;
                    } else {
                        return Err(format!("Could not restore variables: variables are not actually sequestrated"));
                    }
                }
                Instruction::SequestrateVariables => {
                    let cloned = self.variables.clone();
                    self.variables.clear();
                    self.sequestrated_variables.push(cloned);
                }
                Instruction::GetReadFileHandleStack => {
                    match self.stack.pop() {
                        Some(Value::String(name)) => {
                            let res = File::open(&name);
                            match res {
                                Ok(file) => {
                                    self.file_handles.push_back((file, false));
                                    self.file_handles_names.push_back(name.clone());
                                    let len = self.file_handles.len();
                                    let file_pointer = &mut self.file_handles.get_mut(len - 1).unwrap().0 as *mut File;
                                    self.stack.push(Value::FileHandle((file_pointer, false)));
                                }
                                Err(err) => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "Error creating read file handle: {}", err
                                        )));
                                    } else {
                                        return Err(format!(
                                            "Error creating read file handle: {}", err
                                        ));
                                    }
                                }
                            }
                        }
                        Some(_) => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "Expected a string for opening the file handle"
                                )));
                            } else {
                                return Err(format!(
                                    "Expected a string for opening the file handle"
                                ));
                            }
                        }
                        None => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "The stack is empty"
                                )));
                            } else {
                                return Err(format!(
                                    "The stack is empty"
                                ));
                            }
                        }
                    }
                }
                Instruction::GetWriteFileHandleStack => {
                    match self.stack.pop() {
                        Some(Value::String(name)) => {
                            let res = File::options().read(true).write(true).create(true).open(&name);
                            match res {
                                Ok(file) => {
                                    self.file_handles.push_back((file, true));
                                    self.file_handles_names.push_back(name.clone());
                                    let len = self.file_handles.len();
                                    let file_pointer = &mut self.file_handles.get_mut(len - 1).unwrap().0 as *mut File;
                                    self.stack.push(Value::FileHandle((file_pointer, true)));
                                }
                                Err(err) => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "Error creating write file handle: {}", err
                                        )));
                                    } else {
                                        return Err(format!(
                                            "Error creating write file handle: {}", err
                                        ));
                                    }
                                }
                            }
                        }
                        Some(_) => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "Expected a string for opening the file handle"
                                )));
                            } else {
                                return Err(format!(
                                    "Expected a string for opening the file handle"
                                ));
                            }
                        }
                        None => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "The stack is empty"
                                )));
                            } else {
                                return Err(format!(
                                    "The stack is empty"
                                ));
                            }
                        }
                    }
                }
                Instruction::CloseFileHandleStack => {
                    match self.stack.pop() {
                        Some(Value::String(name)) => {
                            let indexof = self.file_handles_names.binary_search(&name);
                            match indexof {
                                Ok(index) => {
                                    let _handle = self.file_handles.remove(index).unwrap();
                                    self.file_handles_names.remove(index);
                                }
                                Err(_) => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "The file handle for '{}' is not opened.", name
                                        )));
                                    } else {
                                        return Err(format!(
                                            "The file handle for '{}' is not opened.", name
                                        ));
                                    }
                                }
                            }
                        }
                        Some(_) => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "Expected a string for closening the file handle"
                                )));
                            } else {
                                return Err(format!(
                                    "Expected a string for closening the file handle"
                                ));
                            }
                        }
                        None => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "The stack is empty"
                                )));
                            } else {
                                return Err(format!(
                                    "The stack is empty"
                                ));
                            }
                        }
                    }
                }
                Instruction::ReadFromFileHandleStack => {
                    match self.stack.pop() {
                        Some(Value::Int(bytes)) => {
                            if bytes < 1 {
                                if self.is_catching {
                                    self.stack.push(Value::Error(format!(
                                        "Cannot read negative bytes: {}", bytes
                                    )));
                                } else {
                                    return Err(format!(
                                        "Cannot read negative bytes: {}", bytes
                                    ));
                                }
                            }
                            match self.stack.pop() {
                                Some(Value::FileHandle((handle, _))) => {
                                    let mut reference = unsafe { handle.as_mut().unwrap() };
                                    let mut buffer = Vec::with_capacity(bytes as usize);
                                    let res = reference.read_exact(&mut buffer);
                                    match res {
                                        Ok(_) => {
                                            self.stack.push(Value::Bytes(buffer));
                                        }
                                        Err(e) => {
                                            if self.is_catching {
                                                self.stack.push(Value::Error(format!(
                                                    "Error reading {} bytes from file handle: {}", bytes, e
                                                )));
                                            } else {
                                                return Err(format!(
                                                    "Error reading {} bytes from file handle: {}", bytes, e
                                                ));
                                            }
                                        }
                                    }
                                }
                                Some(_) => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "Tried to read from a non-File handle object"
                                        )));
                                    } else {
                                        return Err(format!(
                                            "Tried to read from a non-File handle object"
                                        ));
                                    }
                                }
                                None => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "The stack is empty"
                                        )));
                                    } else {
                                        return Err(format!(
                                            "The stack is empty"
                                        ));
                                    }
                                }
                            }
                        }
                        Some(_) => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "Expected a int for reading the file handle"
                                )));
                            } else {
                                return Err(format!(
                                    "Expected a int for reading the file handle"
                                ));
                            }
                        }
                        None => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "The stack is empty"
                                )));
                            } else {
                                return Err(format!(
                                    "The stack is empty"
                                ));
                            }
                        }
                    }
                }
                Instruction::PushFileHandlePointerStack => {
                    match self.stack.pop() {
                        Some(Value::String(name)) => {
                            let indexof = self.file_handles_names.binary_search(&name);
                            match indexof {
                                Ok(index) => {
                                    let (handle, is_writeable) = self.file_handles.get_mut(index).unwrap();
                                    self.stack.push(Value::FileHandle((handle as *mut File, *is_writeable)));
                                }
                                Err(_) => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "The file handle for '{}' is not opened.", name
                                        )));
                                    } else {
                                        return Err(format!(
                                            "The file handle for '{}' is not opened.", name
                                        ));
                                    }
                                }
                            }
                        }
                        Some(_) => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "Expected a string for closening the file handle"
                                )));
                            } else {
                                return Err(format!(
                                    "Expected a string for closening the file handle"
                                ));
                            }
                        }
                        None => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "The stack is empty"
                                )));
                            } else {
                                return Err(format!(
                                    "The stack is empty"
                                ));
                            }
                        }
                    }
                }
                Instruction::DefineClass(name) => {
                    let name = name.clone();
                    let mut object = Class {
                        name: name.clone(),
                        public_methods: FxHashMap::default(),
                        private_methods: FxHashMap::default(),
                        static_methods: FxHashMap::default(),
                        public_properties: FxHashMap::default(),
                        private_properties: FxHashMap::default(),
                    };
                    self.pc += 1;
                    let mut has_defined_public_methods = false;
                    let mut has_defined_private_methods = false;
                    let mut has_defined_public_properties = false;
                    let mut has_defined_private_properties = false;
                    let mut has_defined_static_methods = false;
                    while let Some(instruction) = self.instructions.get(self.pc as usize) {
                        self.pc += 1;
                        eprintln!("{:?}", instruction);
                        match instruction {
                            Instruction::PublicFields => {
                                if has_defined_public_properties {
                                    return Err(format!("Redefinition of public fields inside a class is not allowed"));
                                }
                                has_defined_public_methods = true;
                                loop {
                                    match self.instructions.get(self.pc as usize) {
                                        Some(Instruction::DefineField(name, type_of_field)) => {
                                            self.pc += 1;
                                            object.public_properties.insert(name.to_owned(), (Value::None, type_of_field.clone()));
                                        }
                                        Some(Instruction::EndPublicFields) => {
                                            self.pc += 1;
                                            break;
                                        }
                                        _ => {
                                            return Err(format!("Formatation error: unclosed class public fields definition"))
                                        }
                                    }
                                }
                            }
                            Instruction::PrivateFields => {
                                if has_defined_private_properties {
                                    return Err(format!("Redefinition of private fields inside a class is not allowed"));
                                }
                                has_defined_private_properties = true;
                                self.pc += 1;
                                loop {
                                    match self.instructions.get(self.pc as usize) {
                                        Some(Instruction::DefineField(name, type_of_field)) => {
                                            self.pc += 1;
                                            object.private_properties.insert(name.to_owned(), (Value::None, type_of_field.clone()));
                                        }
                                        Some(Instruction::EndPrivateFields) => {
                                            self.pc += 1;
                                            break;
                                        }
                                        _ => {
                                            return Err(format!("Formatation error: unclosed class private fields definition"))
                                        }
                                    }
                                }
                            }
                            Instruction::PublicMethods => {
                                if has_defined_public_methods {
                                    return Err(format!("Redefinition of public methods inside a class is not allowed"));
                                }
                                has_defined_public_methods = true;
                                self.pc += 1;
                                loop {
                                    match self.instructions.get(self.pc as usize) {
                                        Some(Instruction::ClassMethodDefinition(name, args, return_type)) => {
                                            self.pc += 1;
                                            let mut body: Vec<Instruction> = vec![];
                                            body.push(Instruction::SequestrateVariables);
                                            body.push(Instruction::SequestrateVariables);
                                            body.push(Instruction::Push(Value::String(format!("{}.{}", object.name, name))));
                                            body.push(Instruction::PopToRoot(String::from("__function__")));
                                            self.pc += 1;
                                            while let Some(instruction) = self.instructions.get(self.pc as usize) {
                                                match instruction {
                                                    Instruction::EndFunction => {
                                                        self.pc += 1;
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
                                            if body.len() == 4 {
                                                return Err(format!("Function does not return"));
                                            }
                                            if body.last().unwrap() != &Instruction::Return {
                                                body.push(Instruction::Return);
                                            }
                                            object.public_methods.insert(name.clone(), Function::Interpreted(
                                                FunctionStruct {
                                                    name: name.clone(),
                                                    args: args.clone(),
                                                    returns: return_type.clone(),
                                                    body,
                                                }
                                            ));
                                        }
                                        Some(Instruction::EndPublicMethods) => {
                                            self.pc += 1;
                                            break;
                                        }
                                        _ => {
                                            return Err(format!("Formatation error: unclosed class public fields definition"))
                                        }
                                    }
                                }
                            }
                            Instruction::PrivateMethods => {
                                if has_defined_private_methods {
                                    return Err(format!("Redefinition of private methods inside a class is not allowed"));
                                }
                                has_defined_private_methods = true;
                                self.pc += 1;
                                loop {
                                    match self.instructions.get(self.pc as usize) {
                                        Some(Instruction::ClassMethodDefinition(name, args, return_type)) => {
                                            let mut body: Vec<Instruction> = vec![];
                                            body.push(Instruction::SequestrateVariables);
                                            body.push(Instruction::SequestrateVariables);
                                            body.push(Instruction::Push(Value::String(format!("{}.{}", object.name, name))));
                                            body.push(Instruction::PopToRoot(String::from("__function__")));
                                            self.pc += 1;
                                            while let Some(instruction) = self.instructions.get(self.pc as usize) {
                                                match instruction {
                                                    Instruction::EndFunction => {
                                                        self.pc += 1;
                                                        break;
                                                    }
                                                    Instruction::StartFunction(..) | Instruction::ClassMethodDefinition(..) => {
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
                                            if body.len() == 4 {
                                                return Err(format!("Function does not return"));
                                            }
                                            if body.last().unwrap() != &Instruction::Return {
                                                body.push(Instruction::Return);
                                            }
                                            object.private_methods.insert(name.clone(), Function::Interpreted(
                                                FunctionStruct {
                                                    name: name.clone(),
                                                    args: args.clone(),
                                                    returns: return_type.clone(),
                                                    body,
                                                }
                                            ));
                                        }
                                        Some(Instruction::EndPublicMethods) => {
                                            break;
                                        }
                                        _ => {
                                            return Err(format!("Formatation error: unclosed class private fields definition"))
                                        }
                                    }
                                }
                            }
                            Instruction::InheritFrom(from) => {
                                panic!("Inheritance is currently not implemented.")
                            }
                            Instruction::ConstructorFunctionDefinition(args, return_type) => {
                                let mut body: Vec<Instruction> = vec![];
                                body.push(Instruction::Push(Value::String(format!("{}.__new__", object.name))));
                                body.push(Instruction::PopToRoot(String::from("__function__")));
                                self.pc += 1;
                                while let Some(instruction) = self.instructions.get(self.pc as usize) {
                                    match instruction {
                                        Instruction::EndFunction => {
                                            self.pc += 1;
                                            break;
                                        }
                                        Instruction::StartFunction(..) | Instruction::ClassMethodDefinition(..) => {
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
                                if body.len() == 4 {
                                    return Err(format!("Function does not return"));
                                }
                                if body.last().unwrap() != &Instruction::Return {
                                    body.push(Instruction::Return);
                                }
                                object.static_methods.insert(String::from("__new__"), Function::Interpreted(
                                    FunctionStruct {
                                        name: String::from("__new__"),
                                        args: args.clone(),
                                        returns: return_type.clone(),
                                        body,
                                    }
                                ));
                            }
                            Instruction::StaticMethods => {
                                if has_defined_static_methods {
                                    return Err(format!("Redefinition of static methods inside a class is not allowed"));
                                }
                                has_defined_static_methods = true;
                                self.pc += 1;
                                loop {
                                    match self.instructions.get(self.pc as usize) {
                                        Some(Instruction::StartFunction(name, args, return_type)) => {
                                            let mut body: Vec<Instruction> = vec![];
                                            body.push(Instruction::SequestrateVariables);
                                            body.push(Instruction::SequestrateVariables);
                                            body.push(Instruction::Push(Value::String(format!("{}.{}", object.name, name))));
                                            body.push(Instruction::PopToRoot(String::from("__function__")));
                                            self.pc += 1;
                                            while let Some(instruction) = self.instructions.get(self.pc as usize) {
                                                match instruction {
                                                    Instruction::EndFunction => {
                                                        break;
                                                    }
                                                    Instruction::StartFunction(..) | Instruction::ClassMethodDefinition(..) => {
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
                                            if body.len() == 4 {
                                                return Err(format!("Function does not return"));
                                            }
                                            if body.last().unwrap() != &Instruction::Return {
                                                body.push(Instruction::Return);
                                            }
                                            object.static_methods.insert(name.clone(), Function::Interpreted(
                                                FunctionStruct {
                                                    name: name.clone(),
                                                    args: args.clone(),
                                                    returns: return_type.clone(),
                                                    body,
                                                }
                                            ));
                                        }
                                        Some(Instruction::EndPublicMethods) => {
                                            break;
                                        }
                                        _ => {
                                            return Err(format!("Formatation error: unclosed class private fields definition"))
                                        }
                                    }
                                }
                            }
                            Instruction::EndClass => {
                                self.pc -= 1;
                                break;
                            }
                            _ => {
                                return Err(format!("Formatation error: unexpected instruction inside a class definition: {:?}", instruction))
                            }
                        }
                    }
                    self.classes.insert(name.clone(), object);
                }
                Instruction::Instantiate(name) => {
                    match self.classes.get_mut(name) {
                        Some(class) => {
                            self.current_object = Some(class as *mut Class);
                            self.instructions
                                        .drain((self.pc - 2) as usize..(self.pc as usize));
                            let mut other_part = self
                                .instructions
                                .drain((self.pc as usize - 1)..)
                                .collect::<VecDeque<Instruction>>();
                            self.instructions.push_back(Instruction::InvokeStaticMethod(String::from("__new__")));
                            self.instructions.push_back(Instruction::InvokeStaticMethod(String::from("__new__")));
                            self.instructions.push_back(Instruction::InvokeStaticMethod(String::from("__new__")));
                            self.instructions.append(&mut other_part);
                            eprintln!("{:#?}", self.instructions)
                        }
                        None => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "Unable to instantiate unexistant class {}", name
                                )));
                                continue;
                            } else {
                                return Err(format!(
                                    "Unable to instantiate unexistant class {}", name
                                ));
                            }
                        }
                    }
                }
                Instruction::PublicFields | Instruction::DefineField(..)
                | Instruction::EndPublicFields | Instruction::PrivateFields
                | Instruction::EndPrivateFields | Instruction::PublicMethods
                | Instruction::EndPublicMethods | Instruction::PrivateMethods
                | Instruction::EndPrivateMethods | Instruction::StaticMethods
                | Instruction::EndStaticMethods | Instruction::InheritFrom(_) | Instruction::EndClass
                | Instruction::ConstructorFunctionDefinition(..) | Instruction::ClassMethodDefinition(..) => {
                    return Err(format!("The instruction {:?} is not valid outside of a class definition", self.instructions[self.pc as usize]));
                }
                Instruction::LoadFromThisPublic(name) => {
                    match self.current_object {
                        Some(class) => {
                            let reference = unsafe { class.as_mut().unwrap() };
                            match reference.public_properties.get(name) {
                                Some((value, _)) => {
                                    self.stack.push(value.clone());
                                }
                                None => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "Class {} does not have a public property {}", reference.name, name
                                        )));
                                        continue;
                                    } else {
                                        return Err(format!(
                                            "Class {} does not have a public property {}", reference.name, name
                                        ));
                                    }
                                }
                            }
                        }
                        None => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "Unable to load: current object not set"
                                )));
                                continue;
                            } else {
                                return Err(format!(
                                    "Unable to load: current object not set"
                                ));
                            }
                        }
                    }
                }
                Instruction::LoadFromThisPrivate(name) => {
                    match self.current_object {
                        Some(class) => {
                            let reference = unsafe { class.as_mut().unwrap() };
                            match reference.private_properties.get(name) {
                                Some((value, _)) => {
                                    self.stack.push(value.clone());
                                }
                                None => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "Class {} does not have a private property {}", reference.name, name
                                        )));
                                        continue;
                                    } else {
                                        return Err(format!(
                                            "Class {} does not have a private property {}", reference.name, name
                                        ));
                                    }
                                }
                            }
                        }
                        None => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "Unable to load: current object not set"
                                )));
                                continue;
                            } else {
                                return Err(format!(
                                    "Unable to load: current object not set"
                                ));
                            }
                        }
                    }
                }
                Instruction::SetThisPublic(name, value) => {
                    match self.current_object {
                        Some(class) => {
                            let reference = unsafe { class.as_mut().unwrap() };
                            if !reference.public_properties.contains_key(name) {
                                if self.is_catching {
                                    self.stack.push(Value::Error(format!(
                                        "Class {} does not contain a public property {}", reference.name, name
                                    )));
                                    continue;
                                } else {
                                    return Err(format!(
                                        "Class {} does not contain a public property {}", reference.name, name
                                    ));
                                }
                            } else {
                                match reference.public_properties.get(name).unwrap() {
                                    (_, type_) => {
                                        if let Err(err) = type_check_value(vec![value.clone()], vec![(String::from("class_property_placeholder"), type_.clone())]) {
                                            if self.is_catching {
                                                self.stack.push(Value::Error(format!(
                                                    "Error during typechecking property value: {}", err
                                                )));
                                                continue;
                                            } else {
                                                return Err(format!(
                                                    "Error during typechecking property value: {}", err
                                                ));
                                            }
                                        }
                                    } 
                                }
                            }
                        }
                        None => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "Unable to load: current object not set"
                                )));
                                continue;
                            } else {
                                return Err(format!(
                                    "Unable to load: current object not set"
                                ));
                            }
                        }
                    }
                }
                Instruction::SetThisPrivate(name, value) => {
                    match self.current_object {
                        Some(class) => {
                            let reference = unsafe { class.as_mut().unwrap() };
                            if !reference.private_properties.contains_key(name) {
                                if self.is_catching {
                                    self.stack.push(Value::Error(format!(
                                        "Class {} does not contain a private property {}", reference.name, name
                                    )));
                                    continue;
                                } else {
                                    return Err(format!(
                                        "Class {} does not contain a private property {}", reference.name, name
                                    ));
                                }
                            } else {
                                match reference.private_properties.get(name).unwrap() {
                                    (_, type_) => {
                                        if let Err(err) = type_check_value(vec![value.clone()], vec![(String::from("class_property_placeholder"), type_.clone())]) {
                                            if self.is_catching {
                                                self.stack.push(Value::Error(format!(
                                                    "Error during typechecking property value: {}", err
                                                )));
                                                continue;
                                            } else {
                                                return Err(format!(
                                                    "Error during typechecking property value: {}", err
                                                ));
                                            }
                                        }
                                    } 
                                }
                            }
                        }
                        None => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "Unable to load: current object not set"
                                )));
                                continue;
                            } else {
                                return Err(format!(
                                    "Unable to load: current object not set"
                                ));
                            }
                        }
                    }
                }
                Instruction::SetThisStackPublic(name) => {
                    match self.stack.pop() {
                        Some(value) => {
                            match self.current_object {
                                Some(class) => {
                                    let reference = unsafe { class.as_mut().unwrap() };
                                    if !reference.public_properties.contains_key(name) {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!(
                                                "Class {} does not contain a public property {}", reference.name, name
                                            )));
                                            continue;
                                        } else {
                                            return Err(format!(
                                                "Class {} does not contain a public property {}", reference.name, name
                                            ));
                                        }
                                    } else {
                                        match reference.public_properties.get(name).unwrap() {
                                            (_, type_) => {
                                                if let Err(err) = type_check_value(vec![value.clone()], vec![(String::from("class_property_placeholder"), type_.clone())]) {
                                                    if self.is_catching {
                                                        self.stack.push(Value::Error(format!(
                                                            "Error during typechecking property value: {}", err
                                                        )));
                                                        continue;
                                                    } else {
                                                        return Err(format!(
                                                            "Error during typechecking property value: {}", err
                                                        ));
                                                    }
                                                }
                                            } 
                                        }
                                    }
                                }
                                None => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "Unable to load: current object not set"
                                        )));
                                        continue;
                                    } else {
                                        return Err(format!(
                                            "Unable to load: current object not set"
                                        ));
                                    }
                                }
                            }
                        }
                        None => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "The stack is empty"
                                )));
                                continue;
                            } else {
                                return Err(format!(
                                    "The stack is empty"
                                ));
                            }
                        }
                    }
                }
                Instruction::SetThisStackPrivate(name) => {
                    match self.stack.pop() {
                        Some(value) => {
                            match self.current_object {
                                Some(class) => {
                                    let reference = unsafe { class.as_mut().unwrap() };
                                    if !reference.private_properties.contains_key(name) {
                                        if self.is_catching {
                                            self.stack.push(Value::Error(format!(
                                                "Class {} does not contain a private property {}", reference.name, name
                                            )));
                                            continue;
                                        } else {
                                            return Err(format!(
                                                "Class {} does not contain a private property {}", reference.name, name
                                            ));
                                        }
                                    } else {
                                        match reference.private_properties.get(name).unwrap() {
                                            (_, type_) => {
                                                if let Err(err) = type_check_value(vec![value.clone()], vec![(String::from("class_property_placeholder"), type_.clone())]) {
                                                    if self.is_catching {
                                                        self.stack.push(Value::Error(format!(
                                                            "Error during typechecking property value: {}", err
                                                        )));
                                                        continue;
                                                    } else {
                                                        return Err(format!(
                                                            "Error during typechecking property value: {}", err
                                                        ));
                                                    }
                                                }
                                            } 
                                        }
                                    }
                                }
                                None => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "Unable to load: current object not set"
                                        )));
                                        continue;
                                    } else {
                                        return Err(format!(
                                            "Unable to load: current object not set"
                                        ));
                                    }
                                }
                            }
                        }
                        None => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "The stack is empty"
                                )));
                                continue;
                            } else {
                                return Err(format!(
                                    "The stack is empty"
                                ));
                            }
                        }
                    }
                }
                Instruction::SetCurrentObject(name) => {
                    match self.variables.get(name) {
                        Some(value) => {
                            let reference = unsafe { (*value as *mut Value).as_mut().unwrap() };
                            match reference {
                                Value::Class(c) => {
                                    self.current_object = Some(c as *mut Class);
                                    continue;
                                }
                                _ => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "{} is not a class", name
                                        )));
                                        continue;
                                    } else {
                                        return Err(format!(
                                            "{} is not a class", name
                                        ));
                                    }
                                }
                            }
                        }
                        None => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "Cannot set current object to undefined variable {}", name
                                )));
                                continue;
                            } else {
                                return Err(format!(
                                    "Cannot set current object to undefined variable {}", name
                                ));
                            }
                        }
                    }
                }
                Instruction::InvokeStaticMethod(method) => {
                    eprintln!("We got here!");
                    match self.current_object {
                        Some(classptr) => {
                            let reference = unsafe { classptr.as_mut().unwrap() };
                            let func = reference.static_methods.get(method);
                            match func {
                                Some(func) => {
                                    match func {
                                        Function::Native(fun) => {
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
                                        Function::Interpreted(fun) => {
                                            if self.handles_recursion_count {
                                                if fun.name == self.last_runned {
                                                    self.recursive_level += 1;
                                                    if self.recursive_level >= self.max_recursiveness_level {
                                                        return Err(format!("Max recursiveness level depth exceeded: Static function '{}' called itself {} times.", fun.name, self.recursive_level))
                                                    }
                                                } else {
                                                    self.recursive_level = 0;
                                                    self.last_runned = fun.name.clone();
                                                }
                                            }
                                            let mut args: VecDeque<Value> = VecDeque::new();
                                            if self.args_stack.len() != fun.args.len() {
                                                if self.is_catching {
                                                    self.stack
                                                        .push(Value::Error(String::from("The argument stack cannot fulfil the size of the static function's arguments.")));
                                                } else {
                                                    return Err(String::from("The argument stack cannot fulfil the size of the static function's arguments."));
                                                }
                                            }
                                            if fun.args.len() != 0 {
                                                for _ in 0..fun.args.len() - 1 {
                                                    args.push_back(self.args_stack.pop_front().unwrap());
                                                }
                                            }
                                            args.append(&mut self.args_stack);
                                            let typechecks =
                                                type_check_value(args.clone().into(), fun.args.clone());
                                            if let Err(err) = typechecks {
                                                if self.is_catching {
                                                    self.stack.push(Value::Error(err));
                                                } else {
                                                    return Err(err);
                                                }
                                            } else {
                                                self.stack_before = self.stack.clone();
                                                self.expected_return_types.push(fun.returns.clone());
                                                self.returns_to.push(self.pc + 1);
                                                let fun_body_len = fun.body.len();
                                                self.instructions
                                                    .drain(self.pc as usize..(self.pc as usize));
                                                let other_part = self
                                                    .instructions
                                                    .drain((self.pc as usize + 1)..)
                                                    .collect::<Vec<Instruction>>();
                                                self.instructions
                                                    .append(&mut VecDeque::from(fun.body.clone()));
                                                self.instructions.append(&mut VecDeque::from(other_part));
                                                self.block_len = (fun_body_len + 1) as i64;
                                                let zipped = fun.args.clone().into_iter();
                                                let zipped = zipped.zip(args.into_iter());
                                                for ((varname, _), value) in zipped {
                                                    self.allocate_variable(varname, value);
                                                }
                                                self.check_labels_from_pc(fun_body_len);
                                                eprintln!("{:#?}", fun.body);
                                            }
                                        }
                                        _ => {
                                            return Err(format!("How did we get here? Closures cannot be stored as normal functions!"));
                                        }
                                    }
                                }
                                None => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "Class {} doesn't contain static method {}", reference.name, method
                                        )));
                                        continue;
                                    } else {
                                        return Err(format!(
                                            "Class {} doesn't contain static method {}", reference.name, method
                                        ));
                                    }
                                }
                            }
                        }
                        None => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "Current object not set"
                                )));
                                continue;
                            } else {
                                return Err(format!(
                                    "Current object not set"
                                ));
                            }
                        }
                    }
                }
                Instruction::InvokePublicMethod(method) => {
                    match self.current_object {
                        Some(classptr) => {
                            let reference = unsafe { classptr.as_mut().unwrap() };
                            let func = reference.public_methods.get(method);
                            match func {
                                Some(func) => {
                                    match func {
                                        Function::Native(fun) => {
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
                                        Function::Interpreted(fun) => {
                                            if self.handles_recursion_count {
                                                if fun.name == self.last_runned {
                                                    self.recursive_level += 1;
                                                    if self.recursive_level >= self.max_recursiveness_level {
                                                        return Err(format!("Max recursiveness level depth exceeded: Public method '{}' called itself {} times.", fun.name, self.recursive_level))
                                                    }
                                                } else {
                                                    self.recursive_level = 0;
                                                    self.last_runned = fun.name.clone();
                                                }
                                            }
                                            let mut args: VecDeque<Value> = VecDeque::new();
                                            if self.args_stack.len() != fun.args.len() {
                                                if self.is_catching {
                                                    self.stack
                                                        .push(Value::Error(String::from("The argument stack cannot fulfil the size of the public method's arguments.")));
                                                } else {
                                                    return Err(String::from("The argument stack cannot fulfil the size of the public method's arguments."));
                                                }
                                            }
                                            if fun.args.len() != 0 {
                                                for _ in 0..fun.args.len() - 1 {
                                                    args.push_back(self.args_stack.pop_front().unwrap());
                                                }
                                            }
                                            args.append(&mut self.args_stack);
                                            let typechecks =
                                                type_check_value(args.clone().into(), fun.args.clone());
                                            if let Err(err) = typechecks {
                                                if self.is_catching {
                                                    self.stack.push(Value::Error(err));
                                                } else {
                                                    return Err(err);
                                                }
                                            } else {
                                                self.stack_before = self.stack.clone();
                                                self.expected_return_types.push(fun.returns.clone());
                                                self.returns_to.push(self.pc + 1);
                                                let fun_body_len = fun.body.len();
                                                self.instructions
                                                    .drain(self.pc as usize..(self.pc as usize));
                                                let other_part = self
                                                    .instructions
                                                    .drain((self.pc as usize)..)
                                                    .collect::<Vec<Instruction>>();
                                                self.instructions
                                                    .append(&mut VecDeque::from(fun.body.clone()));
                                                self.instructions.append(&mut VecDeque::from(other_part));
                                                self.block_len = fun_body_len as i64;
                                                let zipped = fun.args.clone().into_iter();
                                                let zipped = zipped.zip(args.into_iter());
                                                for ((varname, _), value) in zipped {
                                                    self.allocate_variable(varname, value);
                                                }
                                                self.check_labels_from_pc(fun_body_len);
                                            }
                                        }
                                        _ => {
                                            return Err(format!("How did we get here? Closures cannot be stored as normal functions!"));
                                        }
                                    }
                                }
                                None => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "Class {} doesn't contain public method {}", reference.name, method
                                        )));
                                        continue;
                                    } else {
                                        return Err(format!(
                                            "Class {} doesn't contain public method {}", reference.name, method
                                        ));
                                    }
                                }
                            }
                        }
                        None => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "Current object not set"
                                )));
                                continue;
                            } else {
                                return Err(format!(
                                    "Current object not set"
                                ));
                            }
                        }
                    }
                }
                Instruction::InvokePrivateMethod(method) => {
                    match self.current_object {
                        Some(classptr) => {
                            let reference = unsafe { classptr.as_mut().unwrap() };
                            let func = reference.private_methods.get(method);
                            match func {
                                Some(func) => {
                                    match func {
                                        Function::Native(fun) => {
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
                                        Function::Interpreted(fun) => {
                                            if self.handles_recursion_count {
                                                if fun.name == self.last_runned {
                                                    self.recursive_level += 1;
                                                    if self.recursive_level >= self.max_recursiveness_level {
                                                        return Err(format!("Max recursiveness level depth exceeded: Private method '{}' called itself {} times.", fun.name, self.recursive_level))
                                                    }
                                                } else {
                                                    self.recursive_level = 0;
                                                    self.last_runned = fun.name.clone();
                                                }
                                            }
                                            let mut args: VecDeque<Value> = VecDeque::new();
                                            if self.args_stack.len() != fun.args.len() {
                                                if self.is_catching {
                                                    self.stack
                                                        .push(Value::Error(String::from("The argument stack cannot fulfil the size of the private method's arguments.")));
                                                } else {
                                                    return Err(String::from("The argument stack cannot fulfil the size of the private method's arguments."));
                                                }
                                            }
                                            if fun.args.len() != 0 {
                                                for _ in 0..fun.args.len() - 1 {
                                                    args.push_back(self.args_stack.pop_front().unwrap());
                                                }
                                            }
                                            args.append(&mut self.args_stack);
                                            let typechecks =
                                                type_check_value(args.clone().into(), fun.args.clone());
                                            if let Err(err) = typechecks {
                                                if self.is_catching {
                                                    self.stack.push(Value::Error(err));
                                                } else {
                                                    return Err(err);
                                                }
                                            } else {
                                                self.stack_before = self.stack.clone();
                                                self.expected_return_types.push(fun.returns.clone());
                                                self.returns_to.push(self.pc + 1);
                                                let fun_body_len = fun.body.len();
                                                self.instructions
                                                    .drain(self.pc as usize..(self.pc as usize));
                                                let other_part = self
                                                    .instructions
                                                    .drain((self.pc as usize)..)
                                                    .collect::<Vec<Instruction>>();
                                                self.instructions
                                                    .append(&mut VecDeque::from(fun.body.clone()));
                                                self.instructions.append(&mut VecDeque::from(other_part));
                                                self.block_len = fun_body_len as i64;
                                                let zipped = fun.args.clone().into_iter();
                                                let zipped = zipped.zip(args.into_iter());
                                                for ((varname, _), value) in zipped {
                                                    self.allocate_variable(varname, value);
                                                }
                                                self.check_labels_from_pc(fun_body_len);
                                            }
                                        }
                                        _ => {
                                            return Err(format!("How did we get here? Closures cannot be stored as normal functions!"));
                                        }
                                    }
                                }
                                None => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "Class {} doesn't contain private method {}", reference.name, method
                                        )));
                                        continue;
                                    } else {
                                        return Err(format!(
                                            "Class {} doesn't contain private method {}", reference.name, method
                                        ));
                                    }
                                }
                            }
                        }
                        None => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "Current object not set"
                                )));
                                continue;
                            } else {
                                return Err(format!(
                                    "Current object not set"
                                ));
                            }
                        }
                    }
                }
                Instruction::PushCurrentObject => {
                    match self.current_object {
                        Some(obj) => {
                            let class = unsafe { std::ptr::read_volatile(obj) };
                            self.stack.push(Value::Class(class));
                        }
                        None => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "Current object not set"
                                )));
                                continue;
                            } else {
                                return Err(format!(
                                    "Current object not set"
                                ));
                            }
                        }
                    }
                }
                Instruction::MakeCurrentObjectNone => {
                    self.current_object = None;
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
                        trace
                            .push_str(&format!("\t{}\n", colorize_string(&format!("{:?}", instr))));
                    }
                } else if let Ok(lim) = u64::from_str_radix(&value, 10) {
                    limit = lim as usize;
                    let remaining_instructions =
                        self.instructions.iter().skip(self.pc as usize).take(limit);
                    for instr in remaining_instructions {
                        trace
                            .push_str(&format!("\t{}\n", colorize_string(&format!("{:?}", instr))));
                    }
                } else {
                    if let Err(err) = u64::from_str_radix(&value, 10) {
                        println!("Warning: Invalid enviroment variable Q_STACK_BACKTRACE_LIM value set: can be either a number or full\n\t--> Error parsing number: {}\n\t--> Using default value", err);
                    }
                    let remaining_instructions =
                        self.instructions.iter().skip(self.pc as usize).take(limit);
                    for instr in remaining_instructions {
                        trace
                            .push_str(&format!("\t{}\n", colorize_string(&format!("{:?}", instr))));
                    }
                }
            }
            _ => {
                let remaining_instructions =
                    self.instructions.iter().skip(self.pc as usize).take(limit);
                for instr in remaining_instructions {
                    trace.push_str(&format!("\t{}\n", colorize_string(&format!("{:?}", instr))));
                }
            }
        }

        trace
    }
}
