use super::{parser::{Statement, Expression, Math}, tokens::TokenKind};

pub fn convert(statements: Vec<Statement>) -> String {
    let mut out = String::new();
    out.push_str("istd\n");
    for statement in statements {
        match statement {
            Statement::Assignment(a, b) | Statement::LetDeclaration(a, b) => {
                convert_expr(&mut out, b);
                out.push_str(&format!("pop {}", a));
            }
            Statement::Conditional(a, b) => {
                unimplemented!("conditionals are not yet implemented");
            }
        }
    }
    out
}

pub fn convert_expr(s: &mut String, expr: Expression) {
    match expr {
        Expression::Variable(v) => {
            s.push_str(&format!("ld {}", v));
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
                    s.push_str(&format!("push string {:?}", l));
                }
                TokenKind::BoolLiteral(b) => {
                    s.push_str(&format!("push bool {:?}", b));
                }
                TokenKind::NoneLiteral => {
                    s.push_str(&format!("push None"));
                }
                _ => {
                    unreachable!("literal handling in converter.rs")
                }
            }
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