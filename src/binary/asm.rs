use crate::function::FunctionStructNoName;
use crate::{
    class::Class, function::Function, function::FunctionStruct, vm::Types, Instruction, Value,
};
use fxhash::FxHashMap;
use std::io::Read;
use std::mem::size_of;

pub fn read_instructions<R: Read>(r: &mut R) -> std::io::Result<Vec<Instruction>> {
    let mut instructions: Vec<Instruction> = Vec::new();

    let mut filetag = [0; 4];
    r.read_exact(&mut filetag)?;
    if filetag != [7, 14, 5, 15] {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            "Invalid file header",
        ));
    }

    let mut num_insts = [0; 16];
    r.read_exact(&mut num_insts)?;
    let num_instructions = u128::from_le_bytes(num_insts);

    for _ in 0..num_instructions {
        let instruction = read_instruction(r)?;
        instructions.push(instruction);
    }

    Ok(instructions)
}

pub fn read_instruction<R: Read>(r: &mut R) -> std::io::Result<Instruction> {
    let mut tag = [0u8];
    r.read_exact(&mut tag)?;

    match tag[0] {
        0 => Ok(Instruction::Declare(read_string(r)?, read_value(r)?)),
        1 => Ok(Instruction::GetInput),
        2 => Ok(Instruction::Print),
        3 => Ok(Instruction::Flush),
        4 => Ok(Instruction::PrintErr),
        5 => Ok(Instruction::FlushErr),
        6 => Ok(Instruction::Assign(read_string(r)?, read_value(r)?)),
        7 => Ok(Instruction::AssignTop(read_string(r)?)),
        8 => Ok(Instruction::ThrowError(read_string(r)?)),
        9 => Ok(Instruction::Push(read_value(r)?)),
        10 => Ok(Instruction::Pop(read_string(r)?)),
        11 => Ok(Instruction::Load(read_string(r)?)),
        12 => Ok(Instruction::Jump(read_string(r)?)),
        13 => Ok(Instruction::Label(read_string(r)?)),
        14 => Ok(Instruction::JumpStack(read_string(r)?)),
        15 => Ok(Instruction::Add),
        16 => Ok(Instruction::Sub),
        17 => Ok(Instruction::Mul),
        18 => Ok(Instruction::Div),
        19 => Ok(Instruction::Mod),
        20 => Ok(Instruction::Pow),
        21 => Ok(Instruction::And),
        22 => Ok(Instruction::Or),
        23 => Ok(Instruction::Xor),
        24 => Ok(Instruction::Not),
        25 => Ok(Instruction::Eq),
        26 => Ok(Instruction::Ne),
        27 => Ok(Instruction::Gt),
        28 => Ok(Instruction::Lt),
        29 => Ok(Instruction::Gte),
        30 => Ok(Instruction::Lte),
        31 => Ok(Instruction::Catch),
        32 => Ok(Instruction::EndCatch),
        33 => Ok(Instruction::IncludeStd),
        34 => Ok(Instruction::Invoke(read_string(r)?)),
        35 => Ok(Instruction::ToArgsStack),
        36 => Ok(Instruction::Return),
        37 => Ok(Instruction::GetClassProperty(
            read_string(r)?,
            read_string(r)?,
        )),
        38 => Ok(Instruction::InvokeClassMethod(
            read_string(r)?,
            read_string(r)?,
        )),
        39 => Ok(Instruction::SetClassProperty(
            read_string(r)?,
            read_string(r)?,
            read_value(r)?,
        )),
        40 => Ok(Instruction::ClassHasProperty(
            read_string(r)?,
            read_string(r)?,
        )),
        41 => Ok(Instruction::ClassHasStaticMethod(
            read_string(r)?,
            read_string(r)?,
        )),
        42 => Ok(Instruction::HaltFromStack),
        43 => Ok(Instruction::StartFunction(
            read_string(r)?,
            read_parameter_list(r)?,
            read_type(r)?,
        )),
        44 => Ok(Instruction::EndFunction),
        46 => Ok(Instruction::Duplicate),
        47 => Ok(Instruction::Include(read_string(r)?)),
        48 => Ok(Instruction::LoopStart),
        49 => Ok(Instruction::LoopEnd),
        50 => Ok(Instruction::InlineAssembly(read_string(r)?)),
        51 => Ok(Instruction::DereferenceRaw),
        52 => Ok(Instruction::DebuggingPrintStack),
        53 => Ok(Instruction::MemoryReadVolatile(read_value(r)?)),
        54 => Ok(Instruction::MemoryWriteVolatile(
            read_value(r)?,
            read_value(r)?,
        )),
        55 => Ok(Instruction::Collect),
        56 => Ok(Instruction::EnterScope),
        57 => Ok(Instruction::LeaveScope),
        58 => Ok(Instruction::AsRef),
        59 => Ok(Instruction::HasRefSameLoc),
        60 => Ok(Instruction::RefDifferenceInLoc),
        61 => Ok(Instruction::Typeof),
        62 => Ok(Instruction::IsInstanceof(read_string(r)?)),
        63 => Ok(Instruction::GetFunctionPtr(read_string(r)?)),
        64 => Ok(Instruction::InvokeViaPtr),
        65 => Ok(Instruction::PushFunctionAsClosurePtr(
            read_parameter_list(r)?,
            read_type(r)?,
        )),
        66 => Ok(Instruction::PopToRoot(read_string(r)?)),
        67 => Ok(Instruction::Cast(read_type(r)?)),
        68 => Ok(Instruction::GetReadFileHandle(read_string(r)?)),
        69 => Ok(Instruction::GetWriteFileHandle(read_string(r)?)),
        70 => Ok(Instruction::CloseFileHandle(read_string(r)?)),
        71  => Ok(Instruction::PushFileHandlePointer(read_string(r)?)),
        72 => Ok(Instruction::ReadFromFileHandle(read_usize(r)?)),
        73 => Ok(Instruction::ReadFileHandleToString),
        74 => Ok(Instruction::ReadFileHandleToBytes),
        75 => Ok(Instruction::WriteStringToFileHandle),
        76 => Ok(Instruction::WriteBytesToFileHandle),
        77 => Ok(Instruction::SequestrateVariables),
        78 => Ok(Instruction::RestoreSequestratedVariables),
        79 => Ok(Instruction::GetReadFileHandleStack),
        80 => Ok(Instruction::GetWriteFileHandleStack),
        81 => Ok(Instruction::CloseFileHandleStack),
        82 => Ok(Instruction::ReadFromFileHandleStack),
        83 => Ok(Instruction::PushFileHandlePointerStack),
        84 => Ok(Instruction::DefineClass(read_string(r)?)),
        85 => Ok(Instruction::PublicFields),
        86 => Ok(Instruction::DefineField(read_string(r)?, read_type(r)?)),
        87 => Ok(Instruction::EndPublicFields),
        88 => Ok(Instruction::PrivateFields),
        89 => Ok(Instruction::EndPrivateFields),
        90 => Ok(Instruction::LoadFromThisPublic(read_string(r)?)),
        91 => Ok(Instruction::LoadFromThisPrivate(read_string(r)?)),
        92 => Ok(Instruction::SetThisPublic(read_string(r)?, read_value(r)?)),
        93 => Ok(Instruction::SetThisPublic(read_string(r)?, read_value(r)?)),
        94 => Ok(Instruction::SetThisStackPublic(read_string(r)?)),
        95 => Ok(Instruction::SetThisStackPrivate(read_string(r)?)),
        96 => Ok(Instruction::ClassMethodDefinition(read_string(r)?, read_parameter_list(r)?, read_type(r)?)),
        97 => Ok(Instruction::PublicMethods),
        98 => Ok(Instruction::EndPublicMethods),
        99 => Ok(Instruction::PrivateMethods),
        100 => Ok(Instruction::EndPrivateMethods),
        101 => Ok(Instruction::StaticMethods),
        102 => Ok(Instruction::EndStaticMethods),
        103 => Ok(Instruction::InheritFrom(read_string(r)?)),
        104 => Ok(Instruction::ConstructorFunctionDefinition(read_parameter_list(r)?, read_type(r)?)),
        105 => Ok(Instruction::Instantiate(read_string(r)?)),
        106 => Ok(Instruction::InvokeStaticMethod(read_string(r)?)),
        107 => Ok(Instruction::InvokePublicMethod(read_string(r)?)),
        108 => Ok(Instruction::InvokePrivateMethod(read_string(r)?)),
        109 => Ok(Instruction::SetCurrentObject(read_string(r)?)),
        110 => Ok(Instruction::EndClass),
        111 => Ok(Instruction::PushCurrentObject),
        112 => Ok(Instruction::MakeCurrentObjectNone),
        _ => Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!("Invalid instruction tag: {:?}", tag[0]),
        )),
    }
}

pub fn read_parameter_list<R: Read>(r: &mut R) -> std::io::Result<Vec<(String, Types)>> {
    let mut params: Vec<(String, Types)> = Vec::new();

    let mut len_buf = [0u8; size_of::<u32>()];
    r.read_exact(&mut len_buf)?;
    let len = u32::from_le_bytes(len_buf);

    for _ in 0..len {
        let name = read_string(r)?;
        let param_type = read_type(r)?;
        params.push((name, param_type));
    }

    Ok(params)
}

pub fn read_string<R: Read>(r: &mut R) -> std::io::Result<String> {
    let mut len_buf = [0u8; size_of::<u32>()];
    r.read_exact(&mut len_buf)?;
    let len = u32::from_le_bytes(len_buf);
    let mut string_buf = vec![0u8; len as usize];
    r.read_exact(&mut string_buf)?;
    let string = String::from_utf8_lossy(string_buf.as_slice()).to_string();
    Ok(string)
}

pub fn read_list<R: Read>(r: &mut R) -> std::io::Result<Vec<Value>> {
    let mut len_buf = [0u8; size_of::<u32>()];
    r.read_exact(&mut len_buf)?;
    let len = u32::from_le_bytes(len_buf);
    let mut list = Vec::with_capacity(len as usize);
    for _ in 0..len {
        list.push(read_value(r)?);
    }
    Ok(list)
}

pub fn read_value<R: Read>(r: &mut R) -> std::io::Result<Value> {
    let mut tag = [0u8];
    r.read_exact(&mut tag)?;
    match tag {
        [0] => return Ok(Value::None),
        [1] => {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "Classes cannot be written to binary"
            ))
        }
        [2] => {
            let mut buffer = [0; size_of::<i32>()];
            r.read_exact(&mut buffer)?;
            return Ok(Value::Int(i32::from_le_bytes(buffer)));
        }
        [3] => {
            let mut buffer = [0; size_of::<i64>()];
            r.read_exact(&mut buffer)?;
            return Ok(Value::BigInt(i64::from_le_bytes(buffer)));
        }
        [4] => {
            let mut buffer = [0; size_of::<f64>()];
            r.read_exact(&mut buffer)?;
            return Ok(Value::Float(f64::from_le_bytes(buffer)));
        }
        [5] => {
            let mut buffer = [0; size_of::<f32>()];
            r.read_exact(&mut buffer)?;
            return Ok(Value::LFloat(f32::from_le_bytes(buffer)));
        }
        [6] => {
            let string = read_string(r)?;
            return Ok(Value::String(string));
        }
        [7] => {
            let mut buffer = [0; size_of::<u32>()];
            r.read_exact(&mut buffer)?;
            return Ok(Value::Character(char::from(buffer[0])));
        }
        [8] => {
            let mut buffer = [0; size_of::<bool>()];
            r.read_exact(&mut buffer)?;
            return Ok(Value::Boolean(buffer[0] != 0));
        }
        [9] => {
            let list = read_list(r)?;
            return Ok(Value::List(list));
        }
        [10] => {
            let tuple = read_list(r)?;
            return Ok(Value::Tuple(tuple));
        }
        [11] => return Ok(Value::Uninitialized),
        [12] => {
            let string = read_string(r)?;
            return Ok(Value::Error(string));
        }
        [13] => {
            let mut buffer = [0; size_of::<u8>()];
            r.read_exact(&mut buffer)?;
            return Ok(Value::Byte(u8::from_le_bytes(buffer)));
        }
        [14] => {
            let mut buffer = [0; size_of::<usize>()];
            r.read_exact(&mut buffer)?;
            let mut bytes = Vec::new();
            for _ in 1..usize::from_le_bytes(buffer) {
                let mut byte_buf = [0; size_of::<u8>()];
                r.read_exact(&mut byte_buf)?;
                bytes.push(u8::from_le_bytes(byte_buf));
            }
            return Ok(Value::Bytes(bytes))
        }
        _ => {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!("Invalid value tag: {:?}", tag),
            ))
        }
    }
}

pub fn read_usize<R: Read>(r: &mut R) -> std::io::Result<usize> {
    let mut buffer = [0; size_of::<usize>()];
    r.read_exact(&mut buffer)?;
    return Ok(usize::from_le_bytes(buffer));
}

pub fn read_type<R: Read>(r: &mut R) -> std::io::Result<Types> {
    let mut tag = [0u8];
    r.read_exact(&mut tag)?;
    match tag {
        [0] => return Ok(Types::None),
        [1] => return Ok(Types::Class),
        [2] => return Ok(Types::Int),
        [3] => return Ok(Types::BigInt),
        [4] => return Ok(Types::Float),
        [5] => return Ok(Types::LFloat),
        [6] => return Ok(Types::String),
        [7] => return Ok(Types::Character),
        [8] => return Ok(Types::Boolean),
        [9] => {
            return Ok(Types::List);
        }
        [10] => {
            return Ok(Types::Tuple);
        }
        [11] => return Ok(Types::Uninitialized),
        [12] => return Ok(Types::Error),
        [13] => return Ok(Types::PtrWrapper),
        [14] => return Ok(Types::Any),
        [15] => return Ok(Types::Function),
        [16] => return Ok(Types::Byte),
        [17] => return Ok(Types::Bytes),
        [18] => return Ok(Types::FileHandle),
        _ => {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                format!("Invalid value tag: {:?}", tag),
            ))
        }
    }
}

pub fn read_static_methods<R: Read>(r: &mut R) -> std::io::Result<FxHashMap<String, Function>> {
    let mut methods = FxHashMap::default();

    let mut len_buf = [0u8; size_of::<u32>()];
    r.read_exact(&mut len_buf)?;
    let len = u32::from_le_bytes(len_buf);

    for _ in 0..len {
        let name = read_string(r)?;
        let function = read_function(r)?;
        methods.insert(name, function);
    }

    Ok(methods)
}

pub fn read_properties<R: Read>(r: &mut R) -> std::io::Result<FxHashMap<String, Value>> {
    let mut properties = FxHashMap::default();

    let mut len_buf = [0u8; size_of::<u32>()];
    r.read_exact(&mut len_buf)?;
    let len = u32::from_le_bytes(len_buf);

    for _ in 0..len {
        let name = read_string(r)?;
        let value = read_value(r)?;
        properties.insert(name, value);
    }

    Ok(properties)
}

pub fn read_function<R: Read>(r: &mut R) -> std::io::Result<Function> {
    let mut tag = [0u8];
    r.read_exact(&mut tag)?;
    match tag {
        [0] => {
            // Native function (unsupported)
            Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "Native functions aren't valid for reading",
            ))
        }
        [1] => {
            // Interpreted function
            let name = read_string(r)?;
            let args = read_parameter_list(r)?;
            let returns = read_type(r)?;
            let body = read_instructions(r)?;
            Ok(Function::Interpreted(FunctionStruct {
                name,
                args,
                returns,
                body,
            }))
        }
        [2] => {
            let args = read_parameter_list(r)?;
            let returns = read_type(r)?;
            let body = read_instructions(r)?;
            Ok(Function::Closure(FunctionStructNoName {
                args,
                returns,
                body,
            }))
        }
        _ => Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            format!("Invalid function tag: {:?}", tag),
        )),
    }
}
