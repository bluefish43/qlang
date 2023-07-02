use std::io::Write;

use crate::{function::Function, vm::Types, Instruction, Value};
use fxhash::FxHashMap;

pub fn write_instructions<W: Write>(
    w: &mut W,
    instructions: Vec<Instruction>,
) -> std::io::Result<()> {
    w.write_all(&[7, 14, 5, 15])?;
    w.write_all(&(instructions.len() as u128).to_le_bytes())?;

    for instruction in instructions {
        write_instruction(w, instruction)?;
    }

    Ok(())
}

pub fn write_instruction<W: Write>(w: &mut W, instruction: Instruction) -> std::io::Result<()> {
    match instruction {
        Instruction::Declare(name, value) => {
            w.write_all(&[0])?;
            write_string(w, name)?;
            write_value(w, value)?;
        }
        Instruction::GetInput => w.write_all(&[1])?,
        Instruction::Print => w.write_all(&[2])?,
        Instruction::Flush => w.write_all(&[3])?,
        Instruction::PrintErr => w.write_all(&[4])?,
        Instruction::FlushErr => w.write_all(&[5])?,
        Instruction::Assign(name, val) => {
            w.write_all(&[6])?;
            write_string(w, name)?;
            write_value(w, val)?;
        }
        Instruction::AssignTop(name) => {
            w.write_all(&[7])?;
            write_string(w, name)?;
        }
        Instruction::ThrowError(err) => {
            w.write_all(&[8])?;
            write_string(w, err)?;
        }
        Instruction::Push(val) => {
            w.write_all(&[9])?;
            write_value(w, val)?;
        }
        Instruction::Pop(name) => {
            w.write_all(&[10])?;
            write_string(w, name)?;
        }
        Instruction::Load(name) => {
            w.write_all(&[11])?;
            write_string(w, name)?;
        }
        Instruction::Jump(name) => {
            w.write_all(&[12])?;
            write_string(w, name)?;
        }
        Instruction::Label(name) => {
            w.write_all(&[13])?;
            write_string(w, name)?;
        }
        Instruction::JumpStack(name) => {
            w.write_all(&[14])?;
            write_string(w, name)?;
        }
        Instruction::Add => w.write_all(&[15])?,
        Instruction::Sub => w.write_all(&[16])?,
        Instruction::Mul => w.write_all(&[17])?,
        Instruction::Div => w.write_all(&[18])?,
        Instruction::Mod => w.write_all(&[19])?,
        Instruction::Pow => w.write_all(&[20])?,
        Instruction::And => w.write_all(&[21])?,
        Instruction::Or => w.write_all(&[22])?,
        Instruction::Xor => w.write_all(&[23])?,
        Instruction::Not => w.write_all(&[24])?,
        Instruction::Eq => w.write_all(&[25])?,
        Instruction::Ne => w.write_all(&[26])?,
        Instruction::Gt => w.write_all(&[27])?,
        Instruction::Lt => w.write_all(&[28])?,
        Instruction::Gte => w.write_all(&[29])?,
        Instruction::Lte => w.write_all(&[30])?,
        Instruction::Catch => w.write_all(&[31])?,
        Instruction::EndCatch => w.write_all(&[32])?,
        Instruction::IncludeStd => w.write_all(&[33])?,
        Instruction::Invoke(name) => {
            w.write_all(&[34])?;
            write_string(w, name)?;
        }
        Instruction::ToArgsStack => w.write_all(&[35])?,
        Instruction::Return => w.write_all(&[36])?,
        Instruction::GetClassProperty(classname, name) => {
            w.write_all(&[37])?;
            write_string(w, classname)?;
            write_string(w, name)?;
        }
        Instruction::InvokeClassMethod(classname, name) => {
            w.write_all(&[38])?;
            write_string(w, classname)?;
            write_string(w, name)?;
        }
        Instruction::SetClassProperty(classname, name, val) => {
            w.write_all(&[39])?;
            write_string(w, classname)?;
            write_string(w, name)?;
            write_value(w, val)?;
        }
        Instruction::ClassHasProperty(classname, name) => {
            w.write_all(&[40])?;
            write_string(w, classname)?;
            write_string(w, name)?;
        }
        Instruction::ClassHasStaticMethod(classname, name) => {
            w.write_all(&[41])?;
            write_string(w, classname)?;
            write_string(w, name)?;
        }
        Instruction::HaltFromStack => w.write_all(&[42])?,
        Instruction::StartFunction(name, params, returns) => {
            w.write_all(&[43])?;
            write_string(w, name)?;
            write_parameter_list(w, params)?;
            write_type(w, returns)?;
        }
        Instruction::EndFunction => w.write_all(&[44])?,
        Instruction::Duplicate => {
            w.write_all(&[46])?;
        }
        Instruction::Include(path) => {
            w.write_all(&[47])?;
            write_string(w, path)?;
        }
        Instruction::LoopStart => {
            w.write_all(&[48])?;
        }
        Instruction::LoopEnd => {
            w.write_all(&[49])?;
        }
        Instruction::InlineAssembly(ins) => {
            w.write_all(&[50])?;
            write_string(w, ins)?;
        }
        Instruction::DereferenceRaw => {
            w.write_all(&[51])?;
        }
        Instruction::DebuggingPrintStack => {
            w.write_all(&[52])?;
        }
        Instruction::MemoryReadVolatile(loc) => {
            w.write_all(&[53])?;
            write_value(w, loc)?;
        }
        Instruction::MemoryWriteVolatile(src, dst) => {
            w.write_all(&[54])?;
            write_value(w, src)?;
            write_value(w, dst)?;
        }
        Instruction::Collect => {
            w.write_all(&[55])?;
        }
        Instruction::EnterScope => {
            w.write_all(&[56])?;
        }
        Instruction::LeaveScope => {
            w.write_all(&[57])?;
        }
        Instruction::AsRef => {
            w.write_all(&[58])?;
        }
        Instruction::HasRefSameLoc => {
            w.write_all(&[59])?;
        }
        Instruction::RefDifferenceInLoc => {
            w.write_all(&[60])?;
        }
        Instruction::Typeof => {
            w.write_all(&[61])?;
        }
        Instruction::IsInstanceof(t) => {
            w.write_all(&[62])?;
            write_string(w, t)?;
        }
        Instruction::GetFunctionPtr(f) => {
            w.write_all(&[63])?;
            write_string(w, f)?;
        }
        Instruction::InvokeViaPtr => {
            w.write_all(&[64])?;
        }
        Instruction::PushFunctionAsClosurePtr(args, ret) => {
            w.write_all(&[65])?;
            write_parameter_list(w, args)?;
            write_type(w, ret)?;
        }
        Instruction::PopToRoot(name) => {
            w.write_all(&[66])?;
            write_string(w, name)?;
        }
        Instruction::Cast(t) => {
            w.write_all(&[67])?;
            write_type(w, t)?;
        }
        Instruction::GetReadFileHandle(f) => {
            w.write_all(&[68])?;
            write_string(w, f)?;
        }
        Instruction::GetWriteFileHandle(f) => {
            w.write_all(&[69])?;
            write_string(w, f)?;
        }
        Instruction::CloseFileHandle(f) => {
            w.write_all(&[70])?;
            write_string(w, f)?;
        }
        Instruction::PushFileHandlePointer(f) => {
            w.write_all(&[71])?;
            write_string(w, f)?;
        }
        Instruction::ReadFromFileHandle(s) => {
            w.write_all(&[72])?;
            w.write_all(&s.to_le_bytes())?;
        }
        Instruction::ReadFileHandleToString => {
            w.write_all(&[73])?;
        }
        Instruction::ReadFileHandleToBytes => {
            w.write_all(&[74])?;
        }
        Instruction::WriteStringToFileHandle => {
            w.write_all(&[75])?;
        }
        Instruction::WriteBytesToFileHandle => {
            w.write_all(&[76])?;
        }
        Instruction::SequestrateVariables => {
            w.write_all(&[77])?;
        }
        Instruction::RestoreSequestratedVariables =>{
            w.write_all(&[78])?;
        }
        Instruction::GetReadFileHandleStack => {
            w.write_all(&[79])?;
        }
        Instruction::GetWriteFileHandleStack => {
            w.write_all(&[80])?;
        }
        Instruction::CloseFileHandleStack => {
            w.write_all(&[81])?;
        }
        Instruction::ReadFromFileHandleStack => {
            w.write_all(&[82])?;
        }
        Instruction::PushFileHandlePointerStack => {
            w.write_all(&[83])?;
        }
        Instruction::DefineClass(c) => {
            w.write_all(&[84])?;
            write_string(w, c)?;
        }
        Instruction::PublicFields => {
            w.write_all(&[85])?;
        }
        Instruction::DefineField(name, type_) => {
            w.write_all(&[86])?;
            write_string(w, name)?;
            write_type(w, type_)?;
        }
        Instruction::EndPublicFields => {
            w.write_all(&[87])?;
        }
        Instruction::PrivateFields => {
            w.write_all(&[88])?;
        }
        Instruction::EndPrivateFields => {
            w.write_all(&[89])?;
        }
        Instruction::LoadFromThisPublic(name) => {
            w.write_all(&[90])?;
            write_string(w, name)?;
        }
        Instruction::LoadFromThisPrivate(name) => {
            w.write_all(&[91])?;
            write_string(w, name)?;
        }
        Instruction::SetThisPublic(name, value) => {
            w.write_all(&[92])?;
            write_string(w, name)?;
            write_value(w, value)?;
        }
        Instruction::SetThisPrivate(name, value) => {
            w.write_all(&[93])?;
            write_string(w, name)?;
            write_value(w, value)?;
        }
        Instruction::SetThisStackPublic(name) => {
            w.write_all(&[94])?;
            write_string(w, name)?;
        }
        Instruction::SetThisStackPrivate(name) => {
            w.write_all(&[95])?;
            write_string(w, name)?;
        }
        Instruction::ClassMethodDefinition(name, args, returns) => {
            w.write_all(&[96])?;
            write_string(w, name)?;
            write_parameter_list(w, args)?;
            write_type(w, returns)?;
        }
        Instruction::PublicMethods => {
            w.write_all(&[97])?;
        }
        Instruction::EndPublicMethods => {
            w.write_all(&[98])?;
        }
        Instruction::PrivateMethods => {
            w.write_all(&[99])?;
        }
        Instruction::EndPrivateMethods => {
            w.write_all(&[100])?;
        }
        Instruction::StaticMethods => {
            w.write_all(&[101])?;
        }
        Instruction::EndStaticMethods => {
            w.write_all(&[102])?;
        }
        Instruction::InheritFrom(name) => {
            w.write_all(&[103])?;
            write_string(w, name)?;
        }
        Instruction::ConstructorFunctionDefinition(args, returns) => {
            w.write_all(&[104])?;
            write_parameter_list(w, args)?;
            write_type(w, returns)?;
        }
        Instruction::Instantiate(n) => {
            w.write_all(&[105])?;
            write_string(w, n)?;
        }
        Instruction::InvokeStaticMethod(n) => {
            w.write_all(&[106])?;
            write_string(w, n)?;
        }
        Instruction::InvokePublicMethod(n) => {
            w.write_all(&[107])?;
            write_string(w, n)?;
        }
        Instruction::InvokePrivateMethod(n) => {
            w.write_all(&[108])?;
            write_string(w, n)?;
        }
        Instruction::SetCurrentObject(n) => {
            w.write_all(&[109])?;
            write_string(w, n)?;
        }
        Instruction::EndClass => {
            w.write_all(&[110])?;
        }
        Instruction::PushCurrentObject => {
            w.write_all(&[111])?;
        }
        Instruction::MakeCurrentObjectNone => {
            w.write_all(&[112])?;
        }
        Instruction::AllocArgsToLocal => {
            w.write_all(&[113])?;
        }
        Instruction::DefineCoroutine(name) => {
            w.write_all(&[114])?;
            write_string(w, name)?;
        }
        Instruction::RunCoroutine(name) => {
            w.write_all(&[115])?;
            write_string(w, name)?;
        }
        Instruction::EndCoroutine => {
            w.write_all(&[116])?;
        }
        Instruction::AwaitCoroutineFutureStack => {
            w.write_all(&[117])?;
        }
    }
    Ok(())
}

pub fn write_string<W: Write>(w: &mut W, s: String) -> std::io::Result<()> {
    let len = s.len() as u32;
    w.write_all(&len.to_le_bytes())?;
    w.write_all(s.as_bytes())?;
    Ok(())
}

pub fn write_value<W: Write>(w: &mut W, v: Value) -> std::io::Result<()> {
    match v {
        Value::None => w.write_all(&[0])?,
        Value::Class(_) => {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidData,
                "Classes cannot be written to binary"
            ))
        }
        Value::Int(i) => {
            w.write_all(&[2])?;
            w.write_all(&i.to_le_bytes())?;
        }
        Value::BigInt(i) => {
            w.write_all(&[3])?;
            w.write_all(&i.to_le_bytes())?;
        }
        Value::Float(f) => {
            w.write_all(&[4])?;
            w.write_all(&f.to_le_bytes())?;
        }
        Value::LFloat(f) => {
            w.write_all(&[5])?;
            w.write_all(&f.to_le_bytes())?;
        }
        Value::String(s) => {
            w.write_all(&[6])?;
            write_string(w, s)?;
        }
        Value::Character(c) => {
            w.write_all(&[7])?;
            w.write_all(&[c as u8])?;
        }
        Value::Boolean(b) => {
            w.write_all(&[8])?;
            w.write_all(&[b as u8])?;
        }
        Value::List(l) => {
            w.write_all(&[9])?;
            write_list(w, l)?;
        }
        Value::Tuple(t) => {
            w.write_all(&[10])?;
            write_list(w, t)?;
        }
        Value::Uninitialized => w.write_all(&[11])?,
        Value::Error(e) => {
            w.write_all(&[12])?;
            write_string(w, e)?;
        }
        Value::PtrWrapper(_) | Value::Function(_) | Value::FileHandle(_) | Value::Future(..) => {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "Pointers (function pointers or file handles) are not a static type (they cannot be written into binary).",
            ))
        }
        Value::Byte(b) => {
            w.write_all(&[13])?;
            w.write_all(&b.to_le_bytes())?;
        }
        Value::Bytes(b) => {
            w.write_all(&[14])?;
            w.write_all(&b.len().to_le_bytes())?;
            for bi in b {
                w.write_all(&bi.to_le_bytes())?;
            }
        }
        Value::Mutex(v) => {
            w.write_all(&[15])?;
            write_value(w, v.lock().clone())?;
        }
    }
    Ok(())
}

pub fn write_static_methods<W: Write>(
    w: &mut W,
    methods: FxHashMap<String, Function>,
) -> std::io::Result<()> {
    let len = methods.len() as u32;
    w.write_all(&len.to_le_bytes())?;
    for (name, method) in methods {
        write_string(w, name)?;
        write_function(w, method)?;
    }

    Ok(())
}

pub fn write_properties<W: Write>(
    w: &mut W,
    props: FxHashMap<String, Value>,
) -> std::io::Result<()> {
    let len = props.len() as u32;
    w.write_all(&len.to_le_bytes())?;
    for (name, val) in props {
        write_string(w, name)?;
        write_value(w, val)?;
    }
    Ok(())
}

pub fn write_list<W: Write>(w: &mut W, l: Vec<Value>) -> std::io::Result<()> {
    let len = l.len() as u32;
    w.write_all(&len.to_le_bytes())?;
    for v in l {
        write_value(w, v)?;
    }
    Ok(())
}

pub fn write_parameter_list<W: Write>(
    w: &mut W,
    list: Vec<(String, Types)>,
) -> std::io::Result<()> {
    let len = list.len() as u32;
    w.write_all(&len.to_le_bytes())?;
    for (name, t) in list {
        write_string(w, name)?;
        write_type(w, t)?;
    }
    Ok(())
}

pub fn write_type<W: Write>(w: &mut W, v: Types) -> std::io::Result<()> {
    match v {
        Types::None => w.write_all(&[0])?,
        Types::Class => {
            w.write_all(&[1])?;
        }
        Types::Int => {
            w.write_all(&[2])?;
        }
        Types::BigInt => {
            w.write_all(&[3])?;
        }
        Types::Float => {
            w.write_all(&[4])?;
        }
        Types::LFloat => {
            w.write_all(&[5])?;
        }
        Types::String => {
            w.write_all(&[6])?;
        }
        Types::Character => {
            w.write_all(&[7])?;
        }
        Types::Boolean => {
            w.write_all(&[8])?;
        }
        Types::List => {
            w.write_all(&[9])?;
        }
        Types::Tuple => {
            w.write_all(&[9])?;
        }
        Types::Uninitialized => w.write_all(&[11])?,
        Types::Error => {
            w.write_all(&[12])?;
        }
        Types::PtrWrapper => {
            w.write_all(&[13])?;
        }
        Types::Any => w.write_all(&[14])?,
        Types::Function => w.write_all(&[15])?,
        Types::Byte => w.write_all(&[16])?,
        Types::Bytes => w.write_all(&[17])?,
        Types::FileHandle => w.write_all(&[18])?,
        Types::Mutex => w.write_all(&[19])?,
    }
    Ok(())
}

pub fn write_function<W: Write>(w: &mut W, function: Function) -> std::io::Result<()> {
    match function {
        Function::Interpreted(func) => {
            w.write_all(&[1])?;
            write_string(w, func.name)?;
            write_parameter_list(w, func.args)?;
            write_type(w, func.returns)?;
            Ok(())
        }
        Function::Closure(c) => {
            w.write_all(&[2])?;
            write_parameter_list(w, c.args)?;
            write_type(w, c.returns)?;
            Ok(())
        }
        _ => return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidData,
            "Cannot write internal function to binary"
        ))
    }
}
