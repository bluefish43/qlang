use core::panic;

use super::asmtokens::{Token, TokenType};
use crate::class::Class;
use crate::vm::Instruction;
use crate::vm::Types;
use crate::vm::Value;

pub struct Parser {
    tokens: Vec<Token>,
    pc: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens, pc: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Instruction>, String> {
        let mut instructions = vec![];
        while let Some(ctoken) = self.tokens.get(self.pc) {
            self.pc += 1;
            match &ctoken.token_type {
                TokenType::Keyword(kw) => match kw.as_str() {
                    "decl" => {
                        let name = self.parse_identifier()?;
                        let value = self.parse_value()?;
                        instructions.push(Instruction::Declare(name, value));
                    }
                    "readln" => {
                        instructions.push(Instruction::GetInput);
                    }
                    "putln" => {
                        instructions.push(Instruction::Print);
                    }
                    "fls" => {
                        instructions.push(Instruction::Flush);
                    }
                    "puterr" => {
                        instructions.push(Instruction::PrintErr);
                    }
                    "flserr" => {
                        instructions.push(Instruction::FlushErr);
                    }
                    "assgn" => {
                        let name = self.parse_identifier()?;
                        let value = self.parse_value()?;
                        instructions.push(Instruction::Assign(name, value));
                    }
                    "assgntop" => {
                        let name = self.parse_identifier()?;
                        instructions.push(Instruction::AssignTop(name));
                    }
                    "throw" => {
                        let error = self.parse_string()?;
                        instructions.push(Instruction::ThrowError(error));
                    }
                    "push" => {
                        let value = self.parse_value()?;
                        instructions.push(Instruction::Push(value));
                    }
                    "pop" => {
                        let name = self.parse_identifier()?;
                        instructions.push(Instruction::Pop(name));
                    }
                    "ld" => {
                        let name = self.parse_identifier()?;
                        instructions.push(Instruction::Load(name));
                    }
                    "jmp" => {
                        let label = self.parse_identifier()?;
                        instructions.push(Instruction::Jump(label));
                    }
                    "label" => {
                        let label = self.parse_identifier()?;
                        instructions.push(Instruction::Label(label));
                    }
                    "jmps" => {
                        let label = self.parse_identifier()?;
                        instructions.push(Instruction::JumpStack(label));
                    }
                    "add" => {
                        instructions.push(Instruction::Add);
                    }
                    "sub" => {
                        instructions.push(Instruction::Sub);
                    }
                    "mul" => {
                        instructions.push(Instruction::Mul);
                    }
                    "div" => {
                        instructions.push(Instruction::Div);
                    }
                    "mod" => {
                        instructions.push(Instruction::Mod);
                    }
                    "pow" => {
                        instructions.push(Instruction::Pow);
                    }
                    "and" => {
                        instructions.push(Instruction::And);
                    }
                    "or" => {
                        instructions.push(Instruction::Or);
                    }
                    "xor" => {
                        instructions.push(Instruction::Xor);
                    }
                    "not" => {
                        instructions.push(Instruction::Not);
                    }
                    "eq" => {
                        instructions.push(Instruction::Eq);
                    }
                    "ne" => {
                        instructions.push(Instruction::Ne);
                    }
                    "gt" => {
                        instructions.push(Instruction::Gt);
                    }
                    "lt" => {
                        instructions.push(Instruction::Lt);
                    }
                    "gte" => {
                        instructions.push(Instruction::Gte);
                    }
                    "lte" => {
                        instructions.push(Instruction::Lte);
                    }
                    "cth" => {
                        instructions.push(Instruction::Catch);
                    }
                    "ecth" => {
                        instructions.push(Instruction::EndCatch);
                    }
                    "istd" => {
                        instructions.push(Instruction::IncludeStd);
                    }
                    "incl" => {
                        let path = self.parse_string()?;
                        instructions.push(Instruction::Include(path));
                    }
                    "ivk" => {
                        let function = self.parse_identifier()?;
                        instructions.push(Instruction::Invoke(function));
                    }
                    "tastk" => {
                        instructions.push(Instruction::ToArgsStack);
                    }
                    "ret" => {
                        instructions.push(Instruction::Return);
                    }
                    "getcp" => {
                        let class = self.parse_identifier()?;
                        let property = self.parse_identifier()?;
                        instructions.push(Instruction::GetClassProperty(class, property));
                    }
                    "ivkcm" => {
                        let class = self.parse_identifier()?;
                        let method = self.parse_identifier()?;
                        instructions.push(Instruction::InvokeClassMethod(class, method));
                    }
                    "setcp" => {
                        let class = self.parse_identifier()?;
                        let property = self.parse_identifier()?;
                        let value = self.parse_value()?;
                        instructions.push(Instruction::SetClassProperty(class, property, value));
                    }
                    "chasp" => {
                        let class = self.parse_identifier()?;
                        let property = self.parse_identifier()?;
                        instructions.push(Instruction::ClassHasProperty(class, property));
                    }
                    "chassm" => {
                        let class = self.parse_identifier()?;
                        let method = self.parse_identifier()?;
                        instructions.push(Instruction::ClassHasStaticMethod(class, method));
                    }
                    "hlt" => {
                        instructions.push(Instruction::HaltFromStack);
                    }
                    "fdef" => {
                        let name = self.parse_identifier()?;
                        let arguments = self.parse_function_arguments()?;
                        let return_type = self.parse_type()?;
                        instructions.push(Instruction::StartFunction(name, arguments, return_type));
                    }
                    "fendef" => {
                        instructions.push(Instruction::EndFunction);
                    }
                    "dup" => {
                        instructions.push(Instruction::Duplicate);
                    }
                    "lst" => {
                        instructions.push(Instruction::LoopStart);
                    }
                    "lnd" => {
                        instructions.push(Instruction::LoopEnd);
                    }
                    "inln" => {
                        let content = self.parse_string()?;
                        instructions.push(Instruction::InlineAssembly(content))
                    }
                    "dbgpstk" => {
                        instructions.push(Instruction::DebuggingPrintStack);
                    }
                    "escp" => {
                        instructions.push(Instruction::EnterScope);
                    }
                    "lscp" => {
                        instructions.push(Instruction::LeaveScope);
                    }
                    "clct" => {
                        instructions.push(Instruction::Collect);
                    }
                    "memrvol" => {
                        let loc = self.parse_value()?;
                        instructions.push(Instruction::MemoryReadVolatile(loc));
                    }
                    "memwvol" => {
                        let src = self.parse_value()?;
                        let dst = self.parse_value()?;
                        instructions.push(Instruction::MemoryWriteVolatile(src, dst));
                    }
                    "asrf" => {
                        instructions.push(Instruction::AsRef);
                    }
                    "hrsm" => {
                        instructions.push(Instruction::HasRefSameLoc);
                    }
                    "rdil" => {
                        instructions.push(Instruction::RefDifferenceInLoc);
                    }
                    "tpof" => {
                        instructions.push(Instruction::Typeof);
                    }
                    "iinsof" => {
                        let name = self.parse_identifier()?;
                        instructions.push(Instruction::IsInstanceof(name));
                    }
                    "gfnptr" => {
                        let name = self.parse_identifier()?;
                        instructions.push(Instruction::GetFunctionPtr(name));
                    }
                    "ivptr" => {
                        instructions.push(Instruction::InvokeViaPtr);
                    }
                    "pfacptr" => {
                        let arguments = self.parse_function_arguments()?;
                        let return_type = self.parse_type()?;
                        instructions.push(Instruction::PushFunctionAsClosurePtr(arguments, return_type));
                    }
                    "cst" => {
                        let t = self.parse_type()?;
                        instructions.push(Instruction::Cast(t));
                    }
                    "grfh" => {
                        let s = self.parse_string()?;
                        instructions.push(Instruction::GetReadFileHandle(s));
                    }
                    "gwfh" => {
                        let s = self.parse_string()?;
                        instructions.push(Instruction::GetWriteFileHandle(s));
                    }
                    "cfh" => {
                        let f = self.parse_string()?;
                        instructions.push(Instruction::CloseFileHandle(f));
                    }
                    "pfhp" => {
                        let f = self.parse_string()?;
                        instructions.push(Instruction::PushFileHandlePointer(f));
                    }
                    "rffh" => {
                        let int = self.parse_int()?;
                        if int < 1 {
                            return Err(format!("Cannot read zero or negative bytes from file"));
                        }
                        instructions.push(Instruction::ReadFromFileHandle(int as usize));
                    }
                    "rfhts" => {
                        instructions.push(Instruction::ReadFileHandleToString);
                    }
                    "rfhtb" => {
                        instructions.push(Instruction::ReadFileHandleToBytes);
                    }
                    "wstfh" => {
                        instructions.push(Instruction::WriteStringToFileHandle);
                    }
                    "wbtfh" => {
                        instructions.push(Instruction::WriteBytesToFileHandle);
                    }
                    "svr" => {
                        instructions.push(Instruction::SequestrateVariables);
                    }
                    "rsvr" => {
                        instructions.push(Instruction::RestoreSequestratedVariables);
                    }
                    "grfhs" => {
                        instructions.push(Instruction::GetReadFileHandleStack);
                    }
                    "gwfhs" => {
                        instructions.push(Instruction::GetWriteFileHandleStack);
                    }
                    "cfhs" => {
                        instructions.push(Instruction::CloseFileHandleStack);
                    }
                    "rffhs" => {
                        instructions.push(Instruction::ReadFromFileHandleStack);
                    }
                    "pfphs" => {
                        instructions.push(Instruction::PushFileHandlePointerStack);
                    }
                    "dfclass" => {
                        let name = self.parse_identifier()?;
                        instructions.push(Instruction::DefineClass(name));
                    }
                    "pubfld" => {
                        instructions.push(Instruction::PublicFields);
                    }
                    "dfield" => {
                        let name = self.parse_identifier()?;
                        let type_ = self.parse_type()?;
                        instructions.push(Instruction::DefineField(name, type_));
                    }
                    "epubfl" => {
                        instructions.push(Instruction::EndPublicFields);
                    }
                    "prfl" => {
                        instructions.push(Instruction::PrivateFields);
                    }
                    "eprfl" => {
                        instructions.push(Instruction::EndPrivateFields);
                    }
                    "lftpb" => {
                        let name = self.parse_identifier()?;
                        instructions.push(Instruction::LoadFromThisPublic(name));
                    }
                    "lftpr" => {
                        let name = self.parse_identifier()?;
                        instructions.push(Instruction::LoadFromThisPrivate(name));
                    }
                    "settpb" => {
                        let name = self.parse_identifier()?;
                        let value = self.parse_value()?;
                        instructions.push(Instruction::SetThisPublic(name, value));
                    }
                    "settpr" => {
                        let name = self.parse_identifier()?;
                        let value = self.parse_value()?;
                        instructions.push(Instruction::SetThisPrivate(name, value));
                    }
                    "stspb" => {
                        let name = self.parse_identifier()?;
                        instructions.push(Instruction::SetThisStackPublic(name));
                    }
                    "stspr" => {
                        let name = self.parse_identifier()?;
                        instructions.push(Instruction::SetThisStackPrivate(name));
                    }
                    "clsmdef" => {
                        let name = self.parse_identifier()?;
                        let args = self.parse_function_arguments()?;
                        let returns = self.parse_type()?;
                        instructions.push(Instruction::ClassMethodDefinition(name, args, returns));
                    }
                    "pubmet" => {
                        instructions.push(Instruction::PublicMethods);
                    }
                    "epubmet" => {
                        instructions.push(Instruction::EndPublicMethods);
                    }
                    "privmet" => {
                        instructions.push(Instruction::PrivateMethods);
                    }
                    "eprivmet" => {
                        instructions.push(Instruction::EndPrivateMethods);
                    }
                    "stmt" => {
                        instructions.push(Instruction::StaticMethods);
                    }
                    "estmt" => {
                        instructions.push(Instruction::EndStaticMethods);
                    }
                    "inh" => {
                        let name = self.parse_identifier()?;
                        instructions.push(Instruction::InheritFrom(name));
                    }
                    "constructor" => {
                        let args = self.parse_function_arguments()?;
                        let returns = self.parse_type()?;
                        instructions.push(Instruction::ConstructorFunctionDefinition(args, returns));
                    }
                    "instan" => {
                        let name = self.parse_identifier()?;
                        instructions.push(Instruction::Instantiate(name));
                    }
                    "ivkstmt" => {
                        let name = self.parse_identifier()?;
                        instructions.push(Instruction::InvokeStaticMethod(name));
                    }
                    "ivkpubmt" => {
                        let name = self.parse_identifier()?;
                        instructions.push(Instruction::InvokePublicMethod(name));
                    }
                    "ivkprivmt" => {
                        let name = self.parse_identifier()?;
                        instructions.push(Instruction::InvokePrivateMethod(name));
                    }
                    "sectobj" => {
                        let name = self.parse_identifier()?;
                        instructions.push(Instruction::SetCurrentObject(name));
                    }
                    "eclass" => {
                        instructions.push(Instruction::EndClass);
                    }
                    "pushcobj" => {
                        instructions.push(Instruction::PushCurrentObject);
                    }
                    "mkcobjnone" => {
                        instructions.push(Instruction::MakeCurrentObjectNone);
                    }
                    "allcargslcl" => {
                        instructions.push(Instruction::AllocArgsToLocal);
                    }
                    "defcor" => {
                        let name = self.parse_identifier()?;
                        instructions.push(Instruction::DefineCoroutine(name));
                    }
                    "endcor" => {
                        instructions.push(Instruction::EndCoroutine);
                    }
                    "runcor" => {
                        let name = self.parse_identifier()?;
                        instructions.push(Instruction::RunCoroutine(name));
                    }
                    "awaitcor" => {
                        instructions.push(Instruction::AwaitCoroutineFutureStack);
                    }
                    _ => {
                        return Err(format!(
                            "{}:{}->{}: Invalid keyword '{}'",
                            ctoken.line,
                            ctoken.column,
                            ctoken.length + ctoken.column,
                            kw
                        ))
                    }
                },
                _ => {
                    return Err(format!(
                        "{}:{}->{}: Invalid position for token {:?}",
                        ctoken.line,
                        ctoken.column,
                        ctoken.length + ctoken.column,
                        ctoken.token_type
                    ))
                }
            }
        }
        Ok(instructions)
    }

    fn parse_identifier(&mut self) -> Result<String, String> {
        if let Some(ctoken) = self.tokens.get(self.pc) {
            self.pc += 1;
            match &ctoken.token_type {
                TokenType::Identifier(name) => Ok(name.clone()),
                _ => Err(format!(
                    "{}:{}->{}: Expected an identifier, found {:?}",
                    ctoken.line,
                    ctoken.column,
                    ctoken.length + ctoken.column,
                    ctoken.token_type,
                )),
            }
        } else {
            let tok = self.tokens.get(self.pc - 1).unwrap();
            Err(format!(
                "{}:{}->{}: Unexpected end of tokens",
                tok.line,
                tok.column,
                tok.length + tok.column
            ))
        }
    }

    fn parse_value(&mut self) -> Result<Value, String> {
        if let Some(ctoken) = self.tokens.get(self.pc) {
            self.pc += 1;
            match &ctoken.token_type {
                TokenType::Type(kw) => match kw.as_str() {
                    "None" => Ok(Value::None),
                    "class" => {
                        let class = self.parse_class()?;
                        Ok(Value::Class(class))
                    }
                    "int" => {
                        let value = self.parse_int()?;
                        Ok(Value::Int(value))
                    }
                    "bigint" => {
                        let value = self.parse_bigint()?;
                        Ok(Value::BigInt(value))
                    }
                    "float" => {
                        let value = self.parse_float()?;
                        Ok(Value::Float(value))
                    }
                    "lfloat" => {
                        let value = self.parse_lfloat()?;
                        Ok(Value::LFloat(value))
                    }
                    "string" => {
                        let value = self.parse_string()?;
                        Ok(Value::String(value))
                    }
                    "Error" => {
                        let value = self.parse_string()?;
                        Ok(Value::Error(value))
                    }
                    "char" => {
                        let value = self.parse_char()?;
                        Ok(Value::Character(value))
                    }
                    "bool" => {
                        let value = self.parse_bool()?;
                        Ok(Value::Boolean(value))
                    }
                    "list" => {
                        let values = self.parse_list()?;
                        Ok(Value::List(values))
                    }
                    "tuple" => {
                        let values = self.parse_tuple()?;
                        Ok(Value::Tuple(values))
                    }
                    "uninitialized" => Ok(Value::Uninitialized),
                    "error" => {
                        let error = self.parse_string()?;
                        Ok(Value::Error(error))
                    }
                    "ptrwrapper" => {
                        panic!("Pointers cannot be hard coded")
                    }
                    _ => Err(format!(
                        "{}:{}: Invalid value keyword '{}'",
                        ctoken.line, ctoken.column, kw
                    )),
                },
                _ => Err(format!(
                    "{}:{}->{}: Expected a value keyword, found {:?}",
                    ctoken.line,
                    ctoken.column,
                    ctoken.length + ctoken.column,
                    ctoken.token_type
                )),
            }
        } else {
            let tok = self.tokens.get(self.pc - 1).unwrap();
            Err(format!(
                "{}:{}->{}: Unexpected end of tokens",
                tok.line,
                tok.column,
                tok.column + tok.length
            ))
        }
    }

    fn parse_class(&mut self) -> Result<Class, String> {
        panic!("Classes not implemented")
    }

    fn parse_int(&mut self) -> Result<i32, String> {
        if let Some(ctoken) = self.tokens.get(self.pc) {
            self.pc += 1;
            match &ctoken.token_type {
                TokenType::Int(i) => Ok(*i),
                _ => {
                    return Err(format!(
                        "{}:{}->{}: Unexpected token: {:?} expected token int",
                        ctoken.line,
                        ctoken.column,
                        ctoken.column + ctoken.length,
                        ctoken.token_type
                    ))
                }
            }
        } else {
            let tok = self.tokens.get(self.pc - 1).unwrap();
            return Err(format!(
                "{}:{}->{}: Unexpected end of tokens",
                tok.line,
                tok.column,
                tok.column + tok.length
            ));
        }
    }

    fn parse_bigint(&mut self) -> Result<i64, String> {
        if let Some(ctoken) = self.tokens.get(self.pc) {
            self.pc += 1;
            match &ctoken.token_type {
                TokenType::Int(i) => Ok(*i as i64),
                _ => {
                    return Err(format!(
                        "{}:{}->{}: Unexpected token: {:?} expected token int",
                        ctoken.line,
                        ctoken.column,
                        ctoken.column + ctoken.length,
                        ctoken.token_type
                    ))
                }
            }
        } else {
            let tok = self.tokens.get(self.pc - 1).unwrap();
            return Err(format!(
                "{}:{}->{}: Unexpected end of tokens",
                tok.line,
                tok.column,
                tok.column + tok.length
            ));
        }
    }

    fn parse_float(&mut self) -> Result<f64, String> {
        if let Some(ctoken) = self.tokens.get(self.pc) {
            self.pc += 1;
            match &ctoken.token_type {
                TokenType::Float(i) => Ok(*i),
                _ => {
                    return Err(format!(
                        "{}:{}->{}: Unexpected token: {:?} expected token float",
                        ctoken.line,
                        ctoken.column,
                        ctoken.column + ctoken.length,
                        ctoken.token_type
                    ))
                }
            }
        } else {
            let tok = self.tokens.get(self.pc - 1).unwrap();
            return Err(format!(
                "{}:{}->{}: Unexpected end of tokens",
                tok.line,
                tok.column,
                tok.column + tok.length
            ));
        }
    }

    fn parse_lfloat(&mut self) -> Result<f32, String> {
        if let Some(ctoken) = self.tokens.get(self.pc) {
            self.pc += 1;
            match &ctoken.token_type {
                TokenType::Float(i) => Ok(*i as f32),
                _ => {
                    return Err(format!(
                        "{}:{}->{}: Unexpected token: {:?} expected token float",
                        ctoken.line,
                        ctoken.column,
                        ctoken.column + ctoken.length,
                        ctoken.token_type
                    ))
                }
            }
        } else {
            let tok = self.tokens.get(self.pc - 1).unwrap();
            return Err(format!(
                "{}:{}->{}: Unexpected end of tokens",
                tok.line,
                tok.column,
                tok.column + tok.length
            ));
        }
    }

    fn parse_string(&mut self) -> Result<String, String> {
        if let Some(ctoken) = self.tokens.get(self.pc) {
            self.pc += 1;
            match &ctoken.token_type {
                TokenType::String(s) => Ok(s.clone()),
                _ => {
                    return Err(format!(
                        "{}:{}->{}: Unexpected token: {:?} expected token string",
                        ctoken.line,
                        ctoken.column,
                        ctoken.column + ctoken.length,
                        ctoken.token_type
                    ))
                }
            }
        } else {
            let tok = self.tokens.get(self.pc - 1).unwrap();
            return Err(format!(
                "{}:{}->{}: Unexpected end of tokens",
                tok.line,
                tok.column,
                tok.column + tok.length
            ));
        }
    }

    fn parse_char(&mut self) -> Result<char, String> {
        if let Some(ctoken) = self.tokens.get(self.pc) {
            self.pc += 1;
            match &ctoken.token_type {
                TokenType::Char(c) => Ok(c.clone().chars().next().unwrap_or('\0')),
                _ => {
                    return Err(format!(
                        "{}:{}->{}: Unexpected token: {:?} expected token char",
                        ctoken.line,
                        ctoken.column,
                        ctoken.column + ctoken.length,
                        ctoken.token_type
                    ))
                }
            }
        } else {
            let tok = self.tokens.get(self.pc - 1).unwrap();
            return Err(format!(
                "{}:{}->{}: Unexpected end of tokens",
                tok.line,
                tok.column,
                tok.column + tok.length
            ));
        }
    }

    fn parse_bool(&mut self) -> Result<bool, String> {
        if let Some(ctoken) = self.tokens.get(self.pc) {
            self.pc += 1;
            match &ctoken.token_type {
                TokenType::Boolean(b) => Ok(*b),
                _ => {
                    return Err(format!(
                        "{}:{}->{}: Unexpected token: {:?} expected token bool",
                        ctoken.line,
                        ctoken.column,
                        ctoken.column + ctoken.length,
                        ctoken.token_type
                    ))
                }
            }
        } else {
            let tok = self.tokens.get(self.pc - 1).unwrap();
            return Err(format!(
                "{}:{}->{}: Unexpected end of tokens",
                tok.line,
                tok.column,
                tok.column + tok.length
            ));
        }
    }

    fn parse_list(&mut self) -> Result<Vec<Value>, String> {
        // TODO: Implement list parsing logic
        unimplemented!()
    }

    fn parse_tuple(&mut self) -> Result<Vec<Value>, String> {
        // TODO: Implement tuple parsing logic
        unimplemented!()
    }

    fn parse_function_arguments(&mut self) -> Result<Vec<(String, Types)>, String> {
        if let Some(_) = self.tokens.get(self.pc) {
            let length = self.parse_int()?;
            let mut args = vec![];
            if length > 0 {
                for _ in 0..length {
                    let n = self.parse_identifier()?;
                    let t = self.parse_type()?;
                    args.push((n, t));
                }
            }
            Ok(args)
        } else {
            let tok = self.tokens.get(self.pc - 1).unwrap();
            return Err(format!(
                "{}:{}->{}: Unexpected end of tokens",
                tok.line,
                tok.column,
                tok.column + tok.length
            ));
        }
    }

    fn parse_type(&mut self) -> Result<Types, String> {
        if let Some(ctoken) = self.tokens.get(self.pc) {
            self.pc += 1;
            match &ctoken.token_type {
                TokenType::Type(ttype) => match ttype.as_str() {
                    "None" => Ok(Types::None),
                    "class" => Ok(Types::Class),
                    "int" => Ok(Types::Int),
                    "bigint" => Ok(Types::BigInt),
                    "float" => Ok(Types::Float),
                    "lfloat" => Ok(Types::LFloat),
                    "string" => Ok(Types::String),
                    "character" => Ok(Types::Character),
                    "boolean" => Ok(Types::Boolean),
                    "list" => Ok(Types::List),
                    "tuple" => Ok(Types::Tuple),
                    "uninitialized" => Ok(Types::Uninitialized),
                    "Error" => Ok(Types::Error),
                    "ptrwrapper" => Ok(Types::PtrWrapper),
                    "any" => Ok(Types::Any),
                    _ => {
                        return Err(format!(
                            "{}:{}->{}: Unrecognized type '{}'",
                            ctoken.line,
                            ctoken.column,
                            ctoken.column + ctoken.length,
                            ttype
                        ));
                    }
                },
                _ => {
                    return Err(format!(
                        "{}:{}->{}: Expected Type token, found: '{:?}'",
                        ctoken.line,
                        ctoken.column,
                        ctoken.column + ctoken.length,
                        &ctoken.token_type
                    ));
                }
            }
        } else {
            let tok = self.tokens.get(self.pc - 1).unwrap();
            return Err(format!(
                "{}:{}->{}: Unexpected end of tokens",
                tok.line,
                tok.column,
                tok.column + tok.length
            ));
        }
    }
}
