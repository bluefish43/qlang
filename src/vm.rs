use crate::{
    class::Class,
    function::{get_natives, Function, FunctionStruct},
    gcwrapper::GCWrapper, binary::{asm, asmtokens::tokenize, asmparser::Parser}, memory::allocator::GarbageCollector,
};
use ansi_term::Color;
use fxhash::FxHashMap;
use std::{
    collections::VecDeque,
    fmt::{Debug, Display},
    io::{stderr, stdin, stdout, Write, Read},
    sync::Arc,
    path::PathBuf,
    time::Instant,
    fs::{canonicalize, File},
    os::raw::{c_int, c_long, c_double, c_float, c_char}, ffi::{CString, CStr}, ptr::NonNull,
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

#[derive(Clone, PartialEq)]
pub enum Value {
    None,
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
    PtrWrapper(NonNull<Value>),
    FileHandle((NonNull<File>, bool)),
    Bytes(Vec<u8>),
    Future(u32),
}

unsafe impl Send for Value {}
unsafe impl Sync for Value {}

#[derive(Clone, PartialEq, Debug)]
pub enum Types {
    None,
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
    FileHandle,
    Bytes,
    Future,
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
        Value::PtrWrapper(ptr) => format!("{:#?}", ptr),
        Value::FileHandle(f) => format!("<file>"),
        Value::Bytes(b) => format!("{:?}", b),
        Value::Future(f) => format!("<future>"),
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
        Value::PtrWrapper(_) => format!("ptr"),
        Value::FileHandle(f) => format!("File"),
        Value::Bytes(b) => format!("Bytes"),
        Value::Future(_) => format!("Future"),
    }
}

pub fn typeof_to_string(val: &Types) -> String {
    match val {
        Types::None => "None".to_string(),
        Types::Int => "int".to_string(),
        Types::BigInt => "bigint".to_string(),
        Types::Float => "float".to_string(),
        Types::LFloat => "lfloat".to_string(),
        Types::String => "string".to_owned(),
        Types::Character => "char".to_string(),
        Types::List => format!("{}", "list"),
        Types::Tuple => format!("{}", "tuple"),
        Types::Uninitialized => "uninitialized".to_string(),
        Types::Boolean => "boolean".to_string(),
        Types::Error => "error".to_string(),
        Types::PtrWrapper => format!("ptr"),
        Types::FileHandle => format!("File"),
        Types::Bytes => format!("Bytes"),
        Types::Future => format!("Future"),
        Types::Any => format!("any"),
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
        Value::PtrWrapper(ptr) => {
            format!("{:#?}", ptr)
        }
        Value::FileHandle(f) => format!("<file Type at {:?}", f.0),
        Value::Bytes(b) => format!("{:?}", b),
        Value::Future(_) => format!("Future"),
    }
}

pub fn value_to_debug(val: &Value) -> String {
    match val {
        Value::None => "None".to_string(),
        Value::Int(i) => i.to_string(),
        Value::BigInt(i) => i.to_string(),
        Value::Float(f) => f.to_string(),
        Value::LFloat(f) => f.to_string(),
        Value::String(s) => format!("{:?}", s),
        Value::Character(c) => format!("{:?}", c),
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
        Value::Error(e) => format!("\"Error: {}\"", e),
        Value::PtrWrapper(ptr) => {
            format!("{:#?}", ptr)
        }
        Value::FileHandle(f) => format!("<file Type at {:?}", f.0),
        Value::Bytes(b) => format!("{:?}", b),
        Value::Future(_) => format!("Future"),
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
    GetInput,                     // variable to store value in
    Print,                        // prints top value to the stdout
    Flush,                        // flushes the stdout
    PrintErr,                     // prints to the stderr
    FlushErr,                     // flushes the stderr
    Assign(String, Value),        // assigns value to already existing variable
    AssignTop(String), // assigns value at the top of the self.stack to already existing variable
    ThrowError(String), // throws a new error
    Push(Value),       // pushes a value to the self.stack
    Pop(String),       // pops the top value of the self.stack to a variable
    Load(String),      // loads a variable's value onto the self.stack
    Jump(String),      // jumps to a label
    Label(String),     // defines a label
    JumpStack(String), // jumps to a label if the top value of the self.stack is true
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
    ToArgsStack,    // pops a value from the self.stack and pushes it onto the args self.stack
    Return,
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

    GetReadFileHandle(String),
    GetWriteFileHandle(String),
    CloseFileHandle(String),
    PushFileHandlePointer(String),
    ReadFromFileHandle(u32),
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

    AllocArgsToLocal,

    DefineCoroutine(String),
    EndCoroutine,
    RunCoroutine(String),
    AwaitCoroutineFutureStack,

    ThrowErrorStack,
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

    file_handles_names: VecDeque<String>,
    file_handles: VecDeque<(File, bool)>,

    sequestrated_variables: Vec<FxHashMap<String, *const Value>>,

    last_runned: String,
    recursive_level: u32,
    max_recursiveness_level: u32,
    handles_recursion_count: bool,
    stack_before: Vec<Value>,
    expected_return_types: Vec<Types>,
    args_to_alloc: Vec<FxHashMap<String, Value>>,
    coroutines: FxHashMap<String, Vec<Instruction>>,
    running_coroutines: Vec<Option<std::thread::JoinHandle<Result<Value, String>>>>,
}

unsafe impl Send for VirtualMachine {}
unsafe impl Sync for VirtualMachine {}

impl VirtualMachine {
    pub fn new(mut instructions: Vec<Instruction>, filepath: &String, max_recursiveness_level: u32,
        handles_recursion_count: bool) -> Result<Self, String> {
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
                variables: FxHashMap::default(),
                functions: FxHashMap::default(),
                classes: FxHashMap::default(),
                included: includeds,
                is_catching: false,
                returns_to: vec![],
                block_len: 0,
                gc: GarbageCollector::new(),
                file_handles: VecDeque::new(),
                file_handles_names: VecDeque::new(),
                sequestrated_variables: Vec::new(),
                last_runned: String::new(),
                max_recursiveness_level,
                handles_recursion_count,
                stack_before: Vec::new(),
                expected_return_types: Vec::new(),
                args_to_alloc: Vec::new(),
                recursive_level: 0,
                running_coroutines: Vec::new(),
                coroutines: FxHashMap::default(),
            })
        } else {
            unreachable!()
        }
    }

    pub fn thread_cloned(&self) -> Self {
        Self {
            stack: vec![],
            args_stack: VecDeque::new(),
            labels: FxHashMap::default(),
            instructions: self.instructions.clone(),
            pc: -1,
            loop_addresses: Vec::new(),
            variables: FxHashMap::default(),
            functions: self.functions.clone(),
            classes: self.classes.clone(),
            included: self.included.clone(),
            is_catching: false,
            returns_to: vec![],
            block_len: 0,
            gc: GarbageCollector::new(),
            file_handles: VecDeque::new(),
            file_handles_names: VecDeque::new(),
            last_runned: String::new(),
            recursive_level: 0,
            max_recursiveness_level: self.max_recursiveness_level,
            handles_recursion_count: self.handles_recursion_count,
            sequestrated_variables: Vec::new(),
            stack_before: Vec::new(),
            expected_return_types: Vec::new(),
            args_to_alloc: Vec::new(),
            coroutines: FxHashMap::default(),
            running_coroutines: Vec::new(),
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
            Some(unsafe { &**ptr})
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
        self.gc.collect();
    }

    pub fn check_labels_from_pc(&mut self, length: u32) {
        let mut pos = self.pc;
        for instruction in self.instructions.iter().skip(self.pc as usize - 1) {
            pos += 1;
            if pos as u32 >= length {
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

    pub fn run(&mut self, timeout: Option<u128>) -> Result<Value, String> {
        self.gc.enter_scope();
        let mut has_timeout = false;
        if let Some(_) = timeout {
            has_timeout = true;
        }
        let mut timed = Instant::now();
        while self.pc < (self.instructions.len() - 1).try_into().unwrap() {
            self.pc += 1;
            if has_timeout {
                if timed.elapsed().as_millis() >= timeout.unwrap() {
                    return Err(format!("Timeout exceeded: ran for {}ms", timed.elapsed().as_millis()))
                }
            }
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
                                    "Could not write to stdout: The self.stack is empty".to_string(),
                                ))
                            } else {
                                return Err(
                                    "Could not write to stdout: The self.stack is empty".to_string()
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
                                    "Could not write to stderr: The self.stack is empty".to_string(),
                                ))
                            } else {
                                return Err(
                                    "Could not write to stderr: The self.stack is empty".to_string()
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
                                "Cannot assign to {} from the self.stack because the self.stack is empty"
                                    .to_string(),
                            ))
                        } else {
                            return Err(
                                "Cannot assign to {} from the self.stack because the self.stack is empty"
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
                        self.stack.push(Value::Error(format!("Cannot jump to label {} according to the top self.stack value because the self.stack is empty", label)));
                    } else {
                        return Err(format!("Cannot jump to label {} according to the top self.stack value because the self.stack is empty", label));
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
                            .push(Value::Error(String::from("The self.stack is empty")));
                    } else {
                        return Err(String::from("The self.stack is empty"));
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
                            .push(Value::Error(String::from("The self.stack is empty")));
                    } else {
                        return Err(String::from("The self.stack is empty"));
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
                            .push(Value::Error(String::from("The self.stack is empty")));
                    } else {
                        return Err(String::from("The self.stack is empty"));
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
                            .push(Value::Error(String::from("The self.stack is empty")));
                    } else {
                        return Err(String::from("The self.stack is empty"));
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
                            .push(Value::Error(String::from("The self.stack is empty")));
                    } else {
                        return Err(String::from("The self.stack is empty"));
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
                            .push(Value::Error(String::from("The self.stack is empty")));
                    } else {
                        return Err(String::from("The self.stack is empty"));
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
                            .push(Value::Error(String::from("The self.stack is empty")));
                    } else {
                        return Err(String::from("The self.stack is empty"));
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
                            .push(Value::Error(String::from("The self.stack is empty")));
                    } else {
                        return Err(String::from("The self.stack is empty"));
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
                            .push(Value::Error(String::from("The self.stack is empty")));
                    } else {
                        return Err(String::from("The self.stack is empty"));
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
                            .push(Value::Error(String::from("The self.stack is empty")));
                    } else {
                        return Err(String::from("The self.stack is empty"));
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
                            .push(Value::Error(String::from("The self.stack is empty")));
                    } else {
                        return Err(String::from("The self.stack is empty"));
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
                            .push(Value::Error(String::from("The self.stack is empty")));
                    } else {
                        return Err(String::from("The self.stack is empty"));
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
                            .push(Value::Error(String::from("The self.stack is empty")));
                    } else {
                        return Err(String::from("The self.stack is empty"));
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
                            .push(Value::Error(String::from("The self.stack is empty")));
                    } else {
                        return Err(String::from("The self.stack is empty"));
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
                            .push(Value::Error(String::from("The self.stack is empty")));
                    } else {
                        return Err(String::from("The self.stack is empty"));
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
                                            return Err(format!("Max recursiveness level depth exceeded: Function `{}` called itself {} times.", fun.name, self.recursive_level))
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
                                    let mut new_args = FxHashMap::default();
                                    for ((varname, _), value) in zipped {
                                        new_args.insert(varname, value);
                                    }
                                    self.args_to_alloc.push(new_args);
                                    self.check_labels_from_pc(self.pc as u32);
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
                            .push(Value::Error(String::from("The self.stack is empty")));
                    } else {
                        return Err(String::from("The self.stack is empty"));
                    }
                }
                Instruction::ToArgsStack => {
                    let top = self.stack.pop();
                    if let Some(top) = top {
                        self.args_stack.push_back(top);
                    } else if self.is_catching {
                        self.stack
                            .push(Value::Error(String::from("The self.stack is empty")));
                    } else {
                        return Err(String::from("The self.stack is empty"));
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
                                    let stack_ref: &Vec<Value> = self.stack.as_ref();
                                    if &self.stack_before != stack_ref {
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
                Instruction::HaltFromStack => {
                    let val = self.stack.pop();
                    match val {
                        Some(val) => return Ok(val),
                        None => return Ok(Value::None),
                    }
                }
                Instruction::StartFunction(name, args, return_type) => {
                    let mut body: Vec<Instruction> = vec![];
                    body.push(Instruction::AllocArgsToLocal);
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
                Instruction::Duplicate => {
                    if self.stack.len() == 0 {
                        return Err(format!("Cannot use duplicate on an empty self.stack"))
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
                            let mut new_runtime_proto = VirtualMachine::new(ins, path, self.max_recursiveness_level, self.handles_recursion_count)?;
                            new_runtime_proto.check_labels();
                            new_runtime_proto.run(timeout)?;
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
                    self.loop_addresses.push(self.pc); // Push the loop start address onto the self.stack
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
                                self.stack.push(unsafe { ptr.as_ref().clone() });
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
                                "Cannot dereference pointer because the self.stack is empty"
                            )));
                        } else {
                            return Err(format!(
                                "Cannot dereference pointer because the self.stack is empty"
                            ));
                        }
                    }
                }
                Instruction::DebuggingPrintStack => {
                    eprintln!("{:?}", self.stack);
                }
                Instruction::MemoryReadVolatile(loc) => {
                    if let Value::PtrWrapper(ptr) = loc {
                        unsafe {
                            let value = std::ptr::read_volatile(ptr.as_ptr());
                            self.stack.push(value);
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
                        unsafe {
                            std::ptr::write_volatile(ptr.as_ptr(), src.clone());
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
                        self.stack.push(Value::PtrWrapper(unsafe { NonNull::new_unchecked(&mut val as *mut Value) }));
                        self.stack.push(val);
                        let len = self.stack.len();
                        self.stack.swap(len - 1, len - 2);
                    } else {
                        if self.is_catching {
                            self.stack.push(Value::Error(
                                "Could not convert to pointer: The self.stack is empty".to_string(),
                            ))
                        } else {
                            return Err(
                                "Could not convert to pointer: The self.stack is empty".to_string()
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
                                            "HasRefSameLoc expects two pointers for arguments".to_string(),
                                        ))
                                    } else {
                                        return Err(
                                            "HasRefSameLoc expects two pointers for arguments".to_string(),
                                        );
                                    }
                                }
                            } else {
                                if self.is_catching {
                                    self.stack.push(Value::Error(
                                        "HasRefSameLoc expects two pointers for arguments".to_string(),
                                    ))
                                } else {
                                    return Err(
                                        "HasRefSameLoc expects two pointers for arguments".to_string(),
                                    );
                                }
                            }
                        } else {
                            if self.is_catching {
                                self.stack.push(Value::Error(
                                    "Could not compare pointer's location: The self.stack has only one operand".to_string(),
                                ))
                            } else {
                                return Err(
                                    "Could not compare pointer's location: The self.stack has only one operand".to_string()
                                );
                            }
                        }
                    } else {
                        if self.is_catching {
                            self.stack.push(Value::Error(
                                "Could not compare pointer's location: The self.stack is empty".to_string(),
                            ))
                        } else {
                            return Err(
                                "Could not compare pointer's location: The self.stack is empty".to_string()
                            );
                        }
                    }
                }
                Instruction::RefDifferenceInLoc => {
                    if let Some(ptr) = self.stack.pop() {
                        if let Some(ptr2) = self.stack.pop() {
                            if let Value::PtrWrapper(ptr) = ptr {
                                if let Value::PtrWrapper(ptr2) = ptr2 {
                                    self.stack.push(Value::BigInt(unsafe { (ptr.as_ptr() as *const u8).offset_from(ptr2.as_ptr() as *const u8) } as i64));
                                } else {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(
                                            "HasRefSameLoc expects two pointers for arguments".to_string(),
                                        ))
                                    } else {
                                        return Err(
                                            "HasRefSameLoc expects two pointers for arguments".to_string(),
                                        );
                                    }
                                }
                            } else {
                                if self.is_catching {
                                    self.stack.push(Value::Error(
                                        "HasRefSameLoc expects two pointers for arguments".to_string(),
                                    ))
                                } else {
                                    return Err(
                                        "HasRefSameLoc expects two pointers for arguments".to_string(),
                                    );
                                }
                            }
                        } else {
                            if self.is_catching {
                                self.stack.push(Value::Error(
                                    "Could not compare pointer's location: The self.stack has only one operand".to_string(),
                                ))
                            } else {
                                return Err(
                                    "Could not compare pointer's location: The self.stack has only one operand".to_string()
                                );
                            }
                        }
                    } else {
                        if self.is_catching {
                            self.stack.push(Value::Error(
                                "Could not compare pointer's location: The self.stack is empty".to_string(),
                            ))
                        } else {
                            return Err(
                                "Could not compare pointer's location: The self.stack is empty".to_string()
                            );
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
                                "Couldn't apply typeof operator: The self.stack is empty".to_string(),
                            ))
                        } else {
                            return Err(
                                "Couldn't apply typeof operator: The self.stack is empty".to_string(),
                            );
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
                            self.stack.push(Value::FileHandle((NonNull::new(file_pointer).unwrap(), false)));
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
                            self.stack.push(Value::FileHandle((NonNull::new(file_pointer).unwrap(), true)));
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
                                    "The file handle for `{}` is not opened.", name
                                )));
                            } else {
                                return Err(format!(
                                    "The file handle for `{}` is not opened.", name
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
                            self.stack.push(Value::FileHandle((NonNull::new(handle).unwrap(), *is_writeable)));
                        }
                        Err(_) => {
                            if self.is_catching {
                                self.stack.push(Value::Error(format!(
                                    "The file handle for `{}` is not opened.", name
                                )));
                            } else {
                                return Err(format!(
                                    "The file handle for `{}` is not opened.", name
                                ));
                            }
                        }
                    }
                }
                Instruction::ReadFromFileHandle(bytes) => {
                    match self.stack.pop() {
                        Some(Value::FileHandle((handle, _))) => {
                            let reference = unsafe { handle.as_ptr().as_mut().unwrap() };
                            let mut buffer = Vec::with_capacity(*bytes as usize);
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
                                    "The self.stack is empty (at reading {} bytes from file handle with dynamic name from self.stack)", bytes
                                )));
                            } else {
                                return Err(format!(
                                    "The self.stack is empty (at reading {} bytes from file handle with dynamic name from self.stack)", bytes
                                ));
                            }
                        }
                    }
                }
                Instruction::ReadFileHandleToString => {
                    match self.stack.pop() {
                        Some(Value::FileHandle((handle, _))) => {
                            let reference = unsafe { handle.as_ptr().as_mut().unwrap() };
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
                                    "The self.stack is empty (at reading from file handle with dynamic name from self.stack to string)"
                                )));
                            } else {
                                return Err(format!(
                                    "The self.stack is empty (at reading from file handle with dynamic name from self.stack to string)"
                                ));
                            }
                        }
                    }
                }
                Instruction::ReadFileHandleToBytes => {
                    match self.stack.pop() {
                        Some(Value::FileHandle((handle, _))) => {
                            let reference = unsafe { handle.as_ptr().as_mut().unwrap() };
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
                                    "The self.stack is empty (at reading from file handle with dynamic name from self.stack to bytes)"
                                )));
                            } else {
                                return Err(format!(
                                    "The self.stack is empty (at reading from file handle with dynamic name from self.stack to bytes)"
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
                                        let reference = unsafe { handle.as_ptr().as_mut().unwrap() };
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
                                            "The self.stack is empty (at collecting file handle from self.stack)"
                                        )));
                                    } else {
                                        return Err(format!(
                                            "The self.stack is empty (at collecting file handle from self.stack)"
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
                                    "The self.stack is empty (at collecting string from self.stack)"
                                )));
                            } else {
                                return Err(format!(
                                    "The self.stack is empty (at collecting string from self.stack)"
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
                                        let reference = unsafe { handle.as_ptr().as_mut().unwrap() };
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
                                            "The self.stack is empty (at collecting file handle from self.stack)"
                                        )));
                                    } else {
                                        return Err(format!(
                                            "The self.stack is empty (at collecting file handle from self.stack)"
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
                                    "The self.stack is empty (at collecting string from self.stack)"
                                )));
                            } else {
                                return Err(format!(
                                    "The self.stack is empty (at collecting string from self.stack)"
                                ));
                            }
                        }
                    }
                }
                Instruction::RestoreSequestratedVariables => {
                    if let Some(mut v) = self.sequestrated_variables.pop() {
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
                                    self.stack.push(Value::FileHandle((NonNull::new(file_pointer).unwrap(), false)));
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
                                    "The self.stack is empty (at getting read file handle for file from self.stack)"
                                )));
                            } else {
                                return Err(format!(
                                    "The self.stack is empty (at getting read file handle for file from self.stack)"
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
                                    self.stack.push(Value::FileHandle((NonNull::new(file_pointer).unwrap(), true)));
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
                                    "The self.stack is empty (at getting write file handle for file from self.stack)"
                                )));
                            } else {
                                return Err(format!(
                                    "The self.stack is empty (at getting write file handle for file from self.stack)"
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
                                            "The file handle for `{}` is not opened.", name
                                        )));
                                    } else {
                                        return Err(format!(
                                            "The file handle for `{}` is not opened.", name
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
                                    "The self.stack is empty (at closing file handle for file from self.stack)"
                                )));
                            } else {
                                return Err(format!(
                                    "The self.stack is empty (at closing file handle for file from self.stack)"
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
                                    let reference = unsafe { handle.as_ptr().as_mut().unwrap() };
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
                                            "The self.stack is empty (at reading from self.stack file handle)"
                                        )));
                                    } else {
                                        return Err(format!(
                                            "The self.stack is empty (at reading from self.stack file handle)"
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
                                    "The self.stack is empty (at reading from self.stack file handle)"
                                )));
                            } else {
                                return Err(format!(
                                    "The self.stack is empty (at reading from self.stack file handle)"
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
                                    self.stack.push(Value::FileHandle((NonNull::new(handle).unwrap(), *is_writeable)));
                                }
                                Err(_) => {
                                    if self.is_catching {
                                        self.stack.push(Value::Error(format!(
                                            "The file handle for `{}` is not opened.", name
                                        )));
                                    } else {
                                        return Err(format!(
                                            "The file handle for `{}` is not opened.", name
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
                                    "The self.stack is empty (at pushing file handle to the self.stack by its name)"
                                )));
                            } else {
                                return Err(format!(
                                    "The self.stack is empty (at pushing file handle to the self.stack by its name)"
                                ));
                            }
                        }
                    }
                }
                Instruction::AllocArgsToLocal => {
                    if let Some(args) = self.args_to_alloc.pop() {
                        for (name, value) in args {
                            self.allocate_variable(name, value);
                        }
                    } else {
                        return Err(format!("No arguments to allocate"));
                    }
                }
                Instruction::DefineCoroutine(name) => {
                    let mut body: Vec<Instruction> = vec![];
                    self.pc += 1;
                    while let Some(instruction) = self.instructions.get(self.pc as usize) {
                        match instruction {
                            Instruction::EndCoroutine => {
                                break;
                            }
                            Instruction::DefineCoroutine(_) => {
                                return Err(format!(
                                    "Definition of coroutine inside a coroutine is not allowed."
                                ))
                            }
                            _ => {
                                body.push(instruction.clone());
                                self.pc += 1;
                            }
                        }
                    }
                    self.coroutines.insert(name.clone(), body);
                }
                Instruction::EndCoroutine => {
                    return Err(format!("EndCoroutine is not a standalone instruction."));
                }
                Instruction::RunCoroutine(name) => {
                    if let Some(instructions) = self.coroutines.get(name) {
                        let mut copied_handle = self.thread_cloned();
                        copied_handle.instructions = instructions.clone().into();
                        let static_handle: &'static mut VirtualMachine = Box::leak(Box::new(copied_handle));
                        let handle = std::thread::spawn(move || {
                            static_handle.run(timeout)
                        });
                        self.running_coroutines.push(Some(handle));
                        self.stack.push(Value::Future((self.running_coroutines.len() - 1) as u32))
                    } else {
                        return Err(format!("Cannot summon nonexistent coroutine `{}`", name));
                    }
                }
                Instruction::AwaitCoroutineFutureStack => {
                    match self.stack.pop() {
                        Some(Value::Future(index)) => {
                            let coroutine = std::mem::replace(&mut self.running_coroutines[index as usize], None);

                            if let Some(coroutine) = coroutine {
                                // Wait for the thread to complete and return its result
                                match coroutine.join() {
                                    Ok(joined_res) => {

                                        match joined_res {
                                            Ok(value) => {
                                                self.stack.push(value);
                                            }
                                            Err(err) => {
                                                if self.is_catching {
                                                    self.stack.push(Value::Error(err.to_string()));
                                                } else {
                                                    return Err(format!("{}", err));
                                                }
                                            }
                                        }
                                    }
                                    Err(err) => {
                                        return Err(format!("Failed to join coroutine to thread main: {:?}", err));
                                    }
                                }
                            } else {
                                return Err(format!("Awaiting the same Future more than one time is not allowed"));
                            }
                        }
                        Some(value) => {
                            return Err(format!("Awaiting a non-Future type like `{}` is not allowed", value_to_readable(&value)));
                        }
                        None => {
                            return Err(format!("The stack is empty (could not grab Future from stack at awaiting)"));
                        }
                    }
                }
                Instruction::ThrowErrorStack => {
                    match self.stack.pop() {
                        Some(err) => {
                            if self.is_catching {
                                match err {
                                    Value::Error(err) => {
                                        self.stack.push(Value::Error(err.to_string()))
                                    }
                                    _ => {
                                        self.stack.push(Value::Error(err.to_string()))
                                    }
                                }
                            } else {
                                match err {
                                    Value::Error(err) => {
                                        return Err(err.to_string())
                                    }
                                    _ => {
                                        return Err(err.to_string());
                                    }
                                }
                            }
                        }
                        None => {
                            return Err(format!("An error occurred during the handling of another error: The stack is empty (tried to throw an error from the stack)"));
                        }
                    }
                }
            }
        }
        Ok(Value::None)
    }

    pub fn get_opstack_backtrace(&self, mut limit: u32) -> String {
        let mut trace = String::new();
        trace.push_str("Stack backtrace:\n");

        match std::env::var("Q_self.stack_BACKTRACE_LIM") {
            Ok(value) => {
                if value.to_lowercase() == String::from("full") {
                    let remaining_instructions = self.instructions.iter().skip(self.pc as usize);
                    for instr in remaining_instructions {
                        trace.push_str(&format!("\t{}\n", colorize_string(&format!("{:?}", instr))));
                    }
                } else if let Ok(lim) = u64::from_str_radix(&value, 10) {
                    limit = lim as u32;
                    let remaining_instructions = self.instructions.iter().skip(self.pc as usize).take(limit as usize);
                    for instr in remaining_instructions {
                        trace.push_str(&format!("\t{}\n", colorize_string(&format!("{:?}", instr))));
                    }
                } else {
                    if let Err(err) = u64::from_str_radix(&value, 10) {
                        println!("Warning: Invalid enviroment variable Q_self.stack_BACKTRACE_LIM value set: can be either a number or full\n\t--> Error parsing number: {}\n\t--> Using default value", err);
                    }
                    let remaining_instructions = self.instructions.iter().skip(self.pc as usize).take(limit as usize);
                    for instr in remaining_instructions {
                        trace.push_str(&format!("\t{}\n", colorize_string(&format!("{:?}", instr))));
                    }
                }
            }
            _ => {
                let remaining_instructions = self.instructions.iter().skip(self.pc as usize).take(limit as usize);
                for instr in remaining_instructions {
                    trace.push_str(&format!("\t{}\n", colorize_string(&format!("{:?}", instr))));
                }
            }
        }

        trace
    }
}