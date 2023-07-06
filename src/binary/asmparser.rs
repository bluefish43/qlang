use core::panic;

use crate::vm::Instruction;
use crate::vm::Types;
use crate::vm::Value;
use crate::class::Class;
use super::asmtokens::{Token, TokenType};

pub struct Parser {
    tokens: Vec<Token>,
    pc: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            pc: 0,
        }
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
                    },
                    "readln" => {
                        instructions.push(Instruction::GetInput);
                    },
                    "putln" => {
                        instructions.push(Instruction::Print);
                    },
                    "fls" => {
                        instructions.push(Instruction::Flush);
                    },
                    "puterr" => {
                        instructions.push(Instruction::PrintErr);
                    },
                    "flserr" => {
                        instructions.push(Instruction::FlushErr);
                    },
                    "assgn" => {
                        let name = self.parse_identifier()?;
                        let value = self.parse_value()?;
                        instructions.push(Instruction::Assign(name, value));
                    },
                    "assgntop" => {
                        let name = self.parse_identifier()?;
                        instructions.push(Instruction::AssignTop(name));
                    },
                    "throw" => {
                        let error = self.parse_string()?;
                        instructions.push(Instruction::ThrowError(error));
                    },
                    "push" => {
                        let value = self.parse_value()?;
                        instructions.push(Instruction::Push(value));
                    },
                    "pop" => {
                        let name = self.parse_identifier()?;
                        instructions.push(Instruction::Pop(name));
                    },
                    "ld" => {
                        let name = self.parse_identifier()?;
                        instructions.push(Instruction::Load(name));
                    },
                    "jmp" => {
                        let label = self.parse_identifier()?;
                        instructions.push(Instruction::Jump(label));
                    },
                    "label" => {
                        let label = self.parse_identifier()?;
                        instructions.push(Instruction::Label(label));
                    },
                    "jmps" => {
                        let label = self.parse_identifier()?;
                        instructions.push(Instruction::JumpStack(label));
                    },
                    "add" => {
                        instructions.push(Instruction::Add);
                    },
                    "sub" => {
                        instructions.push(Instruction::Sub);
                    },
                    "mul" => {
                        instructions.push(Instruction::Mul);
                    },
                    "div" => {
                        instructions.push(Instruction::Div);
                    },
                    "mod" => {
                        instructions.push(Instruction::Mod);
                    },
                    "pow" => {
                        instructions.push(Instruction::Pow);
                    },
                    "and" => {
                        instructions.push(Instruction::And);
                    },
                    "or" => {
                        instructions.push(Instruction::Or);
                    },
                    "xor" => {
                        instructions.push(Instruction::Xor);
                    },
                    "not" => {
                        instructions.push(Instruction::Not);
                    },
                    "eq" => {
                        instructions.push(Instruction::Eq);
                    },
                    "ne" => {
                        instructions.push(Instruction::Ne);
                    },
                    "gt" => {
                        instructions.push(Instruction::Gt);
                    },
                    "lt" => {
                        instructions.push(Instruction::Lt);
                    },
                    "gte" => {
                        instructions.push(Instruction::Gte);
                    },
                    "lte" => {
                        instructions.push(Instruction::Lte);
                    },
                    "cth" => {
                        instructions.push(Instruction::Catch);
                    },
                    "ecth" => {
                        instructions.push(Instruction::EndCatch);
                    },
                    "istd" => {
                        instructions.push(Instruction::IncludeStd);
                    },
                    "incl" => {
                        let path = self.parse_string()?;
                        instructions.push(Instruction::Include(path));
                    },
                    "ivk" => {
                        let function = self.parse_identifier()?;
                        instructions.push(Instruction::Invoke(function));
                    },
                    "tastk" => {
                        instructions.push(Instruction::ToArgsStack);
                    },
                    "ret" => {
                        instructions.push(Instruction::Return);
                    },
                    "hlt" => {
                        instructions.push(Instruction::HaltFromStack);
                    },
                    "fdef" => {
                        let name = self.parse_identifier()?;
                        let arguments = self.parse_function_arguments()?;
                        let return_type = self.parse_type()?;
                        instructions.push(Instruction::StartFunction(name, arguments, return_type));
                    },
                    "fendef" => {
                        instructions.push(Instruction::EndFunction);
                    },
                    "dup" => {
                        instructions.push(Instruction::Duplicate);
                    },
                    "lst" => {
                        instructions.push(Instruction::LoopStart);
                    },
                    "lend" => {
                        instructions.push(Instruction::LoopEnd);
                    },
                    "inln" => {
                        let content = self.parse_string()?;
                        instructions.push(Instruction::InlineAssembly(content))
                    },
                    "dbgpstk" => {
                        instructions.push(Instruction::DebuggingPrintStack);
                    },
                    "escp" => {
                        instructions.push(Instruction::EnterScope);
                    },
                    "lscp" => {
                        instructions.push(Instruction::LeaveScope);
                    },
                    "clct" => {
                        instructions.push(Instruction::Collect);
                    },
                    "memrvol" => {
                        let loc = self.parse_value()?;
                        instructions.push(Instruction::MemoryReadVolatile(loc));
                    },
                    "memwvol" => {
                        let src = self.parse_value()?;
                        let dst = self.parse_value()?;
                        instructions.push(Instruction::MemoryWriteVolatile(src, dst));
                    },
                    "entscp" => {
                        instructions.push(Instruction::EnterScope)
                    }
                    "lvvscp" => {
                        instructions.push(Instruction::LeaveScope)
                    }
                    "asref" => {
                        instructions.push(Instruction::AsRef)
                    }
                    "hrefsm" => {
                        instructions.push(Instruction::HasRefSameLoc)
                    }
                    "refdlc" => {
                        instructions.push(Instruction::RefDifferenceInLoc)
                    }
                    "tpof" => {
                        instructions.push(Instruction::Typeof)
                    }
                    "grfh" => {
                        let path = self.parse_string()?;
                        instructions.push(Instruction::GetReadFileHandle(path))
                    }
                    "gwfh" => {
                        let path = self.parse_string()?;
                        instructions.push(Instruction::GetWriteFileHandle(path))
                    }
                    "cfh" => {
                        let path = self.parse_string()?;
                        instructions.push(Instruction::CloseFileHandle(path))
                    }
                    "pfhp" => {
                        let path = self.parse_string()?;
                        instructions.push(Instruction::PushFileHandlePointer(path))
                    }
                    "rffh" => {
                        let path = self.parse_int()? as u32;
                        instructions.push(Instruction::ReadFromFileHandle(path))
                    }
                    "rfhts" => {
                        instructions.push(Instruction::ReadFileHandleToString)
                    }
                    "rfhtb" => {
                        instructions.push(Instruction::ReadFileHandleToBytes)
                    }
                    "wstfh" => {
                        instructions.push(Instruction::WriteStringToFileHandle)
                    }
                    "wbtfh" => {
                        instructions.push(Instruction::WriteBytesToFileHandle)
                    }
                    "sv" => {
                        instructions.push(Instruction::SequestrateVariables)
                    }
                    "rsv" => {
                        instructions.push(Instruction::RestoreSequestratedVariables)
                    }
                    "grfhs" => {
                        instructions.push(Instruction::GetReadFileHandleStack)
                    }
                    "gwfhs" => {
                        instructions.push(Instruction::GetWriteFileHandleStack)
                    }
                    "cfhs" => {
                        instructions.push(Instruction::CloseFileHandleStack)
                    }
                    "rffhs" => {
                        instructions.push(Instruction::ReadFromFileHandleStack)
                    }
                    "pfhps" => {
                        instructions.push(Instruction::PushFileHandlePointerStack)
                    }
                    "alatl" => {
                        instructions.push(Instruction::AllocArgsToLocal)
                    }
                    "dfcor" => {
                        let name = self.parse_identifier()?;
                        instructions.push(Instruction::DefineCoroutine(name))
                    }
                    "ecor" => {
                        instructions.push(Instruction::EndCoroutine)
                    }
                    "rcor" => {
                        let name = self.parse_identifier()?;
                        instructions.push(Instruction::RunCoroutine(name))
                    }
                    "awtcorfs" => {
                        instructions.push(Instruction::AwaitCoroutineFutureStack)
                    }
                    "throwstk" => {
                        instructions.push(Instruction::ThrowErrorStack);
                    }
                    _ => return Err(format!("{}:{}->{}: Invalid keyword '{}'", ctoken.line, ctoken.column, ctoken.length + ctoken.column, kw)),
                },
                _ => {
                    return Err(format!(
                        "{}:{}->{}: Invalid position for token {:?}",
                        ctoken.line, ctoken.column, ctoken.length + ctoken.column, ctoken.token_type
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
                TokenType::Identifier(name) => {
                    Ok(name.clone())
                },
                _ => Err(format!(
                    "{}:{}->{}: Expected an identifier, found {:?}",
                    ctoken.line, ctoken.column, ctoken.length + ctoken.column, ctoken.token_type, 
                )),
            }
        } else {
            let tok = self.tokens.get(self.pc - 1).unwrap();
            Err(format!("{}:{}->{}: Unexpected end of tokens", tok.line, tok.column, tok.length + tok.column))
        }
    }

    fn parse_value(&mut self) -> Result<Value, String> {
        if let Some(ctoken) = self.tokens.get(self.pc) {
            self.pc += 1;
            match &ctoken.token_type {
                TokenType::Type(kw) => match kw.as_str() {
                    "None" => Ok(Value::None),
                    "int" => {
                        let value = self.parse_int()?;
                        Ok(Value::Int(value))
                    },
                    "bigint" => {
                        let value = self.parse_bigint()?;
                        Ok(Value::BigInt(value))
                    },
                    "float" => {
                        let value = self.parse_float()?;
                        Ok(Value::Float(value))
                    },
                    "lfloat" => {
                        let value = self.parse_lfloat()?;
                        Ok(Value::LFloat(value))
                    },
                    "string" => {
                        let value = self.parse_string()?;
                        Ok(Value::String(value))
                    },
                    "char" => {
                        let value = self.parse_char()?;
                        Ok(Value::Character(value))
                    },
                    "bool" => {
                        let value = self.parse_bool()?;
                        Ok(Value::Boolean(value))
                    },
                    "list" => {
                        let values = self.parse_list()?;
                        Ok(Value::List(values))
                    },
                    "tuple" => {
                        let values = self.parse_tuple()?;
                        Ok(Value::Tuple(values))
                    },
                    "uninitialized" => Ok(Value::Uninitialized),
                    "error" => {
                        let error = self.parse_string()?;
                        Ok(Value::Error(error))
                    },
                    "ptrwrapper" => {
                        panic!("Pointers cannot be hard coded")
                    },
                    _ => Err(format!("{}:{}: Invalid value keyword '{}'", ctoken.line, ctoken.column, kw)),
                },
                _ => Err(format!(
                    "{}:{}->{}: Expected a value keyword, found {:?}",
                    ctoken.line, ctoken.column, ctoken.length + ctoken.column, ctoken.token_type
                )),
            }
        } else {
            let tok = self.tokens.get(self.pc - 1).unwrap();
            Err(format!("{}:{}->{}: Unexpected end of tokens", tok.line, tok.column, tok.column + tok.length))
        }
    }

    fn parse_class(&mut self) -> Result<Class, String> {
        panic!("Classes not implemented")
    }

    fn parse_int(&mut self) -> Result<i32, String> {
        if let Some(ctoken) = self.tokens.get(self.pc) {
            self.pc += 1;
            match &ctoken.token_type {
                TokenType::Int(i) => {
                    Ok(*i)
                }
                _ => {
                    return Err(format!("{}:{}->{}: Unexpected token: {:?} expected token int", ctoken.line, ctoken.column, ctoken.column + ctoken.length, ctoken.token_type))
                }
            }
        } else {
            let tok = self.tokens.get(self.pc - 1).unwrap();
            return Err(format!("{}:{}->{}: Unexpected end of tokens", tok.line, tok.column, tok.column + tok.length))
        }
    }

    fn parse_bigint(&mut self) -> Result<i64, String> {
        if let Some(ctoken) = self.tokens.get(self.pc) {
            self.pc += 1;
            match &ctoken.token_type {
                TokenType::Int(i) => {
                    Ok(*i as i64)
                }
                _ => {
                    return Err(format!("{}:{}->{}: Unexpected token: {:?} expected token int", ctoken.line, ctoken.column, ctoken.column + ctoken.length, ctoken.token_type))
                }
            }
        } else {
            let tok = self.tokens.get(self.pc - 1).unwrap();
            return Err(format!("{}:{}->{}: Unexpected end of tokens", tok.line, tok.column, tok.column + tok.length))
        }
    }

    fn parse_float(&mut self) -> Result<f64, String> {
        if let Some(ctoken) = self.tokens.get(self.pc) {
            self.pc += 1;
            match &ctoken.token_type {
                TokenType::Float(i) => {
                    Ok(*i)
                }
                _ => {
                    return Err(format!("{}:{}->{}: Unexpected token: {:?} expected token float", ctoken.line, ctoken.column, ctoken.column + ctoken.length, ctoken.token_type))
                }
            }
        } else {
            let tok = self.tokens.get(self.pc - 1).unwrap();
            return Err(format!("{}:{}->{}: Unexpected end of tokens", tok.line, tok.column, tok.column + tok.length))
        }
    }

    fn parse_lfloat(&mut self) -> Result<f32, String> {
        if let Some(ctoken) = self.tokens.get(self.pc) {
            self.pc += 1;
            match &ctoken.token_type {
                TokenType::Float(i) => {
                    Ok(*i as f32)
                }
                _ => {
                    return Err(format!("{}:{}->{}: Unexpected token: {:?} expected token float", ctoken.line, ctoken.column, ctoken.column + ctoken.length, ctoken.token_type))
                }
            }
        } else {
            let tok = self.tokens.get(self.pc - 1).unwrap();
            return Err(format!("{}:{}->{}: Unexpected end of tokens", tok.line, tok.column, tok.column + tok.length))
        }
    }

    fn parse_string(&mut self) -> Result<String, String> {
        if let Some(ctoken) = self.tokens.get(self.pc) {
            self.pc += 1;
            match &ctoken.token_type {
                TokenType::String(s) => {
                    Ok(s.clone())
                }
                _ => {
                    return Err(format!("{}:{}->{}: Unexpected token: {:?} expected token string", ctoken.line, ctoken.column, ctoken.column + ctoken.length, ctoken.token_type))
                }
            }
        } else {
            let tok = self.tokens.get(self.pc - 1).unwrap();
            return Err(format!("{}:{}->{}: Unexpected end of tokens", tok.line, tok.column, tok.column + tok.length))
        }
    }

    fn parse_char(&mut self) -> Result<char, String> {
        if let Some(ctoken) = self.tokens.get(self.pc) {
            self.pc += 1;
            match &ctoken.token_type {
                TokenType::Char(c) => {
                    Ok(c.clone().chars().next().unwrap_or('\0'))
                }
                _ => {
                    return Err(format!("{}:{}->{}: Unexpected token: {:?} expected token char", ctoken.line, ctoken.column, ctoken.column + ctoken.length, ctoken.token_type))
                }
            }
        } else {
            let tok = self.tokens.get(self.pc - 1).unwrap();
            return Err(format!("{}:{}->{}: Unexpected end of tokens", tok.line, tok.column, tok.column + tok.length))
        }
    }

    fn parse_bool(&mut self) -> Result<bool, String> {
        if let Some(ctoken) = self.tokens.get(self.pc) {
            self.pc += 1;
            match &ctoken.token_type {
                TokenType::Boolean(b) => {
                    Ok(*b)
                }
                _ => {
                    return Err(format!("{}:{}->{}: Unexpected token: {:?} expected token bool", ctoken.line, ctoken.column, ctoken.column + ctoken.length, ctoken.token_type))
                }
            }
        } else {
            let tok = self.tokens.get(self.pc - 1).unwrap();
            return Err(format!("{}:{}->{}: Unexpected end of tokens", tok.line, tok.column, tok.column + tok.length))
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

    fn parse_raw_ptr(&mut self) -> Result<*const Value, String> {
        return Err(format!("Hard-coded pointers are not supported"))
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
            return Err(format!("{}:{}->{}: Unexpected end of tokens", tok.line, tok.column, tok.column + tok.length))
        }
    }

    fn parse_type(&mut self) -> Result<Types, String> {
        if let Some(ctoken) = self.tokens.get(self.pc) {
            self.pc += 1;
            match &ctoken.token_type {
                TokenType::Type(ttype) => {
                    match ttype.as_str() {
                        "None" => {
                            Ok(Types::None)
                        }
                        "int" => {
                            Ok(Types::Int)
                        }
                        "bigint" => {
                            Ok(Types::BigInt)
                        }
                        "float" => {
                            Ok(Types::Float)
                        }
                        "lfloat" => {
                            Ok(Types::LFloat)
                        }
                        "string" => {
                            Ok(Types::String)
                        }
                        "character" => {
                            Ok(Types::Character)
                        }
                        "boolean" => {
                            Ok(Types::Boolean)
                        }
                        "list" => {
                            Ok(Types::List)
                        }
                        "tuple" => {
                            Ok(Types::Tuple)
                        }
                        "uninitialized" => {
                            Ok(Types::Uninitialized)
                        }
                        "Error" => {
                            Ok(Types::Error)
                        }
                        "ptrwrapper" => {
                            Ok(Types::PtrWrapper)
                        }
                        "any" => {
                            Ok(Types::Any)
                        }
                        _ => {
                            return Err(format!("{}:{}->{}: Unrecognized type '{}'", ctoken.line, ctoken.column, ctoken.column + ctoken.length, ttype));
                        }
                    }
                }
                _ => {
                    return Err(format!("{}:{}->{}: Expected Type token, found: '{:?}'", ctoken.line, ctoken.column, ctoken.column + ctoken.length, &ctoken.token_type));
                }
            }
        } else {
            let tok = self.tokens.get(self.pc - 1).unwrap();
            return Err(format!("{}:{}->{}: Unexpected end of tokens", tok.line, tok.column, tok.column + tok.length))
        }
    }
}
