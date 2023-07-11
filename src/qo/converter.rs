use super::{parser::{Statement, Expression, Math, QoTypes}, tokens::TokenKind};
use crate::vm::{Types, typeof_to_string};

fn convert_qo_types_to_types(qo_type: QoTypes) -> Types {
    match qo_type {
        QoTypes::I32 => Types::Int,
        QoTypes::I64 => Types::BigInt,
        QoTypes::F32 => Types::LFloat,
        QoTypes::F64 => Types::Float,
        QoTypes::String => Types::String,
        QoTypes::Char => Types::Character,
        QoTypes::Bool => Types::Boolean,
        QoTypes::List => Types::List,
        QoTypes::Tuple => Types::Tuple,
        QoTypes::Uninit => Types::Uninitialized,
        QoTypes::Error => Types::Error,
        QoTypes::Pointer => Types::PtrWrapper,
        QoTypes::File => Types::FileHandle,
        QoTypes::Bytes => Types::Bytes,
        QoTypes::Future => Types::Future,
        QoTypes::Any => Types::Any,
        QoTypes::None => Types::None,
    }
}

pub fn convert(statements: Vec<Statement>) -> String {
    let mut out = String::new();
    out.push_str("istd\n");
    for statement in statements {
        match statement {
            Statement::Assignment(a, b) | Statement::LetDeclaration(a, b) => {
                convert_expr(&mut out, b);
                out.push_str(&format!("pop {}\n", a));
            }
            Statement::Conditional(a, b) => {
                unimplemented!("conditionals are not yet implemented");
            }
            Statement::Expr(e) => {
                convert_expr(&mut out, e);
            }
            Statement::FunctionDefinition(name, args, returns, funstats) => {
                if let Some(args) = args {
                    out.push_str(&format!("fdef {} {} {} {}\n",
                        name,
                        args.len(),
                        convert_args_to_string(args),
                        typeof_to_string(&convert_qo_types_to_types(returns)
                    )));
                    out.push_str(&convert(funstats));
                    out.push_str(&format!("fendef\n"));
                } else {
                    unimplemented!("Variant length arguments are not yet implemented")
                }
            }
            Statement::Return(expr) => {
                convert_expr(&mut out, expr);
                out.push_str("ret\n");
            }
            Statement::StrPush(n) => {
                out.push_str(&format!("push string {:?}", n));
            }
        }
    }
    out
}

pub fn convert_args_to_string(args: Vec<(String, Vec<QoTypes>)>) -> String {
    let mut output = String::new();
    for (name, type_) in args {
        output.push_str(&format!("{} ", name));
        output.push_str(&format!("{}\n", typeof_to_string(&convert_qo_types_to_types(type_[0]))));
    }
    output
}

pub fn convert_expr(s: &mut String, expr: Expression) {
    match expr {
        Expression::Variable(v) => {
            s.push_str(&format!("ld {}\n", v));
        }
        Expression::Numerical(n) => {
            convert_math(s, n);
        }
        Expression::FunctionCall(f, args) => {
            for arg in args {
                convert_expr(s, arg);
                s.push_str("tastk\n");
            }
            s.push_str(&format!("ivk {}\n", f));
        }
        Expression::Literal(l) => {
            match l {
                TokenKind::StringLiteral(l) => {
                    s.push_str(&format!("push string {:?}\n", l));
                }
                TokenKind::BoolLiteral(b) => {
                    s.push_str(&format!("push bool {:?}\n", b));
                }
                TokenKind::NoneLiteral => {
                    s.push_str(&format!("push None\n"));
                }
                _ => {
                    unreachable!("literal handling in converter.rs")
                }
            }
        }
        Expression::List(l) => {
            eprintln!("l");
            s.push_str(&format!("ivk list.new\n"));
            s.push_str(&format!("asref\n"));
            for expr in l {
                s.push_str(&format!("dup\n"));
                s.push_str(&format!("tastk\n"));
                convert_expr(s, expr);
                s.push_str(&format!("tastk\n"));
                s.push_str("ivk list.push\n");
            }
            s.push_str(&format!("pop _\n"));
        }
    }
}

pub fn convert_math(s: &mut String, math: Math) {
    match math {
        Math::Addition(a, b) => {
            convert_math(s, *a);
            convert_math(s, *b);
            s.push_str(&format!("add\n"));
        }
        Math::Subtraction(a, b) => {
            convert_math(s, *a);
            convert_math(s, *b);
            s.push_str(&format!("sub\n"));
        }
        Math::Multiplication(a, b) => {
            convert_math(s, *a);
            convert_math(s, *b);
            s.push_str(&format!("mul\n"));
        }
        Math::Division(a, b) => {
            convert_math(s, *a);
            convert_math(s, *b);
            s.push_str(&format!("div\n"));
        }
        Math::Remainder(a, b) => {
            convert_math(s, *a);
            convert_math(s, *b);
            s.push_str(&format!("mod\n"));
        }
        Math::Int(i) => {
            s.push_str(&format!("push int {}\n", i));
        }
        Math::Float(f) => {
            s.push_str(&format!("push float {}\n", f));
        }
        Math::Expr(e) => {
            convert_expr(s, *e);
        }
    }
}