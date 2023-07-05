use crate::{Instruction, vm::{Value, value_to_debug, value_to_typeof, typeof_to_string}};

pub fn return_instructions_as_string(instructions: Vec<Instruction>) -> String {
    let mut out = String::new();
    for instruction in instructions.iter() {
        match instruction {
            Instruction::Declare(name, value) => {
                out.push_str(&format!("decl {} {} {}\n", name, value_to_typeof(&value), value_to_debug(&value)));
            }
            Instruction::GetInput => {
                out.push_str("readln\n");
            }
            Instruction::Print => {
                out.push_str("putln\n");
            }
            Instruction::Flush => {
                out.push_str("fls\n");
            }
            Instruction::PrintErr => {
                out.push_str("puterr\n");
            }
            Instruction::FlushErr => {
                out.push_str("flserr\n");
            }
            Instruction::Assign(name, value) => {
                out.push_str(&format!("assgn {} {} {}\n", name, value_to_typeof(value), value_to_debug(value)));
            }
            Instruction::AssignTop(name) => {
                out.push_str(&format!("assgntop {}\n", name))
            }
            Instruction::ThrowError(err) => {
                out.push_str(&format!("throw {:?}\n", err))
            }
            Instruction::Push(value) => {
                out.push_str(&format!("push {} {}\n", value_to_typeof(&value), value_to_debug(value)))
            }
            Instruction::Pop(name) => {
                out.push_str(&format!("pop {}\n", name))
            }
            Instruction::Load(name) => {
                out.push_str(&format!("ld {}\n", name))
            }
            Instruction::Jump(label) => {
                out.push_str(&format!("jmp {}\n", label));
            }
            Instruction::JumpStack(name) => {
                out.push_str(&format!("jmps {}\n", name));
            }
            Instruction::Label(l) => {
                out.push_str(&format!("label {}\n", l))
            }
            Instruction::Add => {
                out.push_str("add")
            }
            Instruction::Sub => {
                out.push_str("sub")
            }
            Instruction::Mul => {
                out.push_str("mul\n")
            }
            Instruction::Div => {
                out.push_str("div\n")
            }
            Instruction::Mod => {
                out.push_str("mod\n")
            }
            Instruction::Pow => {
                out.push_str("pow\n")
            }
            Instruction::And => {
                out.push_str("and\n")
            }
            Instruction::Or => {
                out.push_str("or\n")
            }
            Instruction::Xor => {
                out.push_str("xor\n")
            }
            Instruction::Not => {
                out.push_str("not\n")
            }
            Instruction::Eq => {
                out.push_str("eq\n")
            }
            Instruction::Ne => {
                out.push_str("ne\n")
            }
            Instruction::Gt => {
                out.push_str("gt\n")
            }
            Instruction::Lt => {
                out.push_str("lt\n")
            }
            Instruction::Gte => {
                out.push_str("gte\n")
            }
            Instruction::Lte => {
                out.push_str("lte\n")
            }
            Instruction::Catch => {
                out.push_str("cth\n")
            }
            Instruction::EndCatch => {
                out.push_str("ecth\n")
            }
            Instruction::IncludeStd => {
                out.push_str("istd\n")
            }
            Instruction::Include(i) => {
                out.push_str(&format!("incl {:?}\n", i));
            }
            Instruction::Invoke(name) => {
                out.push_str(&format!("ivk {}\n", name));
            }
            Instruction::ToArgsStack => {
                out.push_str("tastk\n");
            }
            Instruction::Return => {
                out.push_str("ret\n");
            }
            Instruction::HaltFromStack => {
                out.push_str("hlt\n");
            }
            Instruction::StartFunction(name, args, returns) => {
                let mut args_str = String::new();
                for (name, type_) in args.iter() {
                    args_str.push_str(&format!("{} {}", name, typeof_to_string(type_)));
                }
                out.push_str(&format!("fdef {} {} {} {}\n", name, args.len(), args_str, typeof_to_string(returns)));
            }
            _ => {
                unimplemented!()
            }
        }
    }
    out
}