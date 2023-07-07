use super::{parser::{Statement, Expression, Math}, tokens::TokenKind};

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
        }
    }
    out
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