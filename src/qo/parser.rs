use core::panic;

use fxhash::FxHashMap;

use super::tokens::{Token, TokenKind};

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    LetDeclaration(String, Expression),
    Assignment(String, Expression),
    Conditional(Condition, Vec<Statement>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Condition {
    Eq(Box<Expression>, Box<Expression>),
    Ne(Box<Expression>, Box<Expression>),
    Gt(Box<Expression>, Box<Expression>),
    Lt(Box<Expression>, Box<Expression>),
    Gte(Box<Expression>, Box<Expression>),
    Lte(Box<Expression>, Box<Expression>),
    Not(Box<Expression>, Box<Expression>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Numerical(Math),
    Literal(TokenKind),
    Variable(String),
    FunctionCall(String, Vec<Expression>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Math {
    Expr(Box<Expression>),
    Int(i64),
    Float(f64),
    Addition(Box<Math>, Box<Math>),
    Subtraction(Box<Math>, Box<Math>),
    Multiplication(Box<Math>, Box<Math>),
    Division(Box<Math>, Box<Math>),
    Remainder(Box<Math>, Box<Math>),
}

#[derive(Clone, Debug, PartialEq, Copy)]
pub enum QoTypes {
    I32,
    I64,
    F32,
    F64,
    String,
    Char,
    Bool,
    List,
    Tuple,
    Uninit,
    Error,
    Pointer,
    File,
    Bytes,
    Future,
    Any,
    None,
}

#[derive(Clone, Debug)]
pub struct Parser {
    statements: Vec<Statement>,
    iter: std::iter::Peekable<std::vec::IntoIter<Token>>,
    variable_types_map: FxHashMap<String, QoTypes>,
    function_types_map: FxHashMap<String, (Vec<QoTypes>, QoTypes)>,
}

impl Parser {
    pub fn new(tokens: &Vec<Token>) -> Parser {
        return Parser {
            statements: Vec::new(),
            iter: tokens.clone().into_iter().peekable(),
            variable_types_map: FxHashMap::default(),
            function_types_map: FxHashMap::default(),
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>, (String, Option<Token>)> {
        loop {
            let next_token = self.iter.next();
            match next_token {
                Some(token) => {
                    match &token.kind {
                        TokenKind::Keyword(k) => {
                            match k.as_str() {
                                "let" => {
                                    eprintln!("{:?}", self.iter);
                                    let name = self.parse_identifier()?;
                                    let variable_type = self.parse_annotation()?;
                                    self.variable_types_map.insert(name.clone(), variable_type);
                                    self.expect_kind(TokenKind::Equals)?;
                                    let expr = self.evaluate_expected(variable_type)?;
                                    self.statements.push(Statement::LetDeclaration(name, expr))
                                }
                                _ => {
                                    panic!("Exhaustive handling of keywords in Parser.parse: {}", k)
                                }
                            }
                        }
                        _ => {
                            return Err((format!("Unexpected location for token `{:?}`", &token), Some(token)))
                        }
                    }
                }
                None => break,
            }
        }
        Ok(self.statements.clone())
    }

    pub fn parse_identifier(&mut self) -> Result<String, (String, Option<Token>)> {
        match self.iter.next() {
            Some(token) => {
                match &token.kind {
                    TokenKind::Identifier(i) => {
                        return Ok(i.to_string())
                    }
                    _ => {
                        return Err((format!("Expected an identifier here, found: {:?}", &token), Some(token)))
                    }
                }
            }
            None => {
                return Err((format!("Expected an identifier here"), None))
            }
        }
    }

    pub fn expect_kind(&mut self, kind: TokenKind) -> Result<(), (String, Option<Token>)> {
        match self.iter.next() {
            Some(token) => {
                if &token.kind != &kind {
                    return Err((format!("Expected token `{:?}`, found `{:?}`", kind, &token.kind), Some(token)))
                } else {
                    Ok(())
                }
            }
            None => {
                return Err((format!("Expected an identifier here, found EOF"), None))
            }
        }
    }

    pub fn evaluate_expected(&mut self, expected: QoTypes) -> Result<Expression, (String, Option<Token>)> {
        let peeked = self.iter.peek().cloned();
        match peeked {
            Some(token) => {
                match &token.kind {
                    TokenKind::NumericLiteral(_) => {
                        if expected != QoTypes::I32 && expected != QoTypes::I64 {
                            return Err((format!("A numeric literal is not compatible with the type {:?}", expected), Some(token)))
                        }
                        let tokens = self.get_while_not_semi();
                        let expr = self.parse_math_expression(tokens);
                        match expr {
                            Some(expr) => {
                                self.expect_kind(TokenKind::Semicolon)?;
                                Ok(Expression::Numerical(expr))
                            }
                            None => {
                                return Err((format!("Error during math expression parsing"), Some(token)))
                            }
                        }
                    }
                    TokenKind::StringLiteral(_) => {
                        if expected != QoTypes::String {
                            return Err((format!("A string literal is not compatible with the type {:?}", expected), Some(token)))
                        }
                        self.expect_kind(TokenKind::Semicolon)?;
                        Ok(Expression::Literal(token.kind))
                    } 
                    TokenKind::FloatLiteral(f) => {
                        if expected != QoTypes::F32 && expected != QoTypes::F64 {
                            return Err((format!("A float literal is not compatible with the type {:?}", expected), Some(token)))
                        }
                        let tokens = self.get_while_not_semi();
                        let expr = self.parse_math_expression(tokens);
                        match expr {
                            Some(expr) => {
                                self.expect_kind(TokenKind::Semicolon)?;
                                Ok(Expression::Numerical(expr))
                            }
                            None => {
                                return Err((format!("Error during math expression parsing"), Some(token)))
                            }
                        }
                    }
                    TokenKind::BoolLiteral(_) => {
                        if expected != QoTypes::Bool {
                            return Err((format!("A bool literal is not compatible with the type {:?}", expected), Some(token)))
                        }
                        self.expect_kind(TokenKind::Semicolon)?;
                        Ok(Expression::Literal(token.kind))
                    }
                    TokenKind::NoneLiteral => {
                        if expected != QoTypes::Bool {
                            return Err((format!("A None literal is not compatible with the type {:?}", expected), Some(token)))
                        }
                        self.expect_kind(TokenKind::Semicolon)?;
                        Ok(Expression::Literal(token.kind))
                    }
                    _ => {
                        return Err((format!("Expected an expression, found `{:?}`", &token.kind), Some(token)))
                    }
                }
            }
            None => {
                return Err((format!("Expected a colon here"), None))
            }
        }
    }

    pub fn parse_annotation(&mut self) -> Result<QoTypes, (String, Option<Token>)> {
        match self.iter.next() {
            Some(token) => {
                match &token.kind {
                    TokenKind::Colon => {
                        match self.iter.next() {
                            Some(token) => {
                                match &token.kind {
                                    TokenKind::Type(t) => {
                                        match t.as_str() {
                                            "i32" => Ok(QoTypes::I32),
                                            "i64" => Ok(QoTypes::I64),
                                            "f32" => Ok(QoTypes::F32),
                                            "f64" => Ok(QoTypes::F64),
                                            "string" => Ok(QoTypes::String),
                                            "char" => Ok(QoTypes::Char),
                                            "bool" => Ok(QoTypes::Bool),
                                            "list" => Ok(QoTypes::List),
                                            "tuple" => Ok(QoTypes::Tuple),
                                            "uninit" => Ok(QoTypes::Uninit),
                                            "Error" => Ok(QoTypes::Error),
                                            "pointer" => Ok(QoTypes::Pointer),
                                            "File" => Ok(QoTypes::File),
                                            "bytes" => Ok(QoTypes::Bytes),
                                            "Future" => Ok(QoTypes::Future),
                                            "any" => Ok(QoTypes::Any),
                                            _ => {
                                                panic!("Unexpected type {}", t)
                                            }
                                        }
                                    }
                                    _ => {
                                        return Err((format!("Expected token `Type`, found `{:?}`", &token.kind), Some(token)))
                                    }
                                }
                            }
                            None => {
                                return Err((format!("Expected a colon here"), None))
                            }
                        }
                    }
                    _ => {
                        return Err((format!("Expected token `Colon`, found `{:?}`", &token.kind), Some(token)))
                    }
                }
            }
            None => {
                return Err((format!("Expected a colon here"), None))
            }
        }
    }

    pub fn get_while_not_semi(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while let Some(token) = self.iter.peek() {
            match &token.kind {
                TokenKind::Semicolon => {
                    break;
                }
                _ => {
                    let token = self.iter.next().unwrap();
                    tokens.push(token);
                }
            }
        }
        tokens
    }

    fn parse_math_expression(&self, tokens: Vec<Token>) -> Option<Math> {
        self.parse_math_expression_helper(&tokens, 0)
    }
    
    fn parse_math_expression_helper(&self, tokens: &[Token], precedence: u8) -> Option<Math> {
        let mut current_index = 0;
        let mut lhs = match tokens.get(current_index) {
            Some(Token { kind: TokenKind::NumericLiteral(num), .. }) => Math::Int(*num),
            Some(Token { kind: TokenKind::FloatLiteral(num), .. }) => Math::Float(*num),
            Some(Token { kind: TokenKind::Identifier(i), .. }) => Math::Expr(Box::new(Expression::Variable(i.to_string()))),
            _ => return None,
        };
    
        current_index += 1;
    
        while current_index < tokens.len() {
            let operator_token = &tokens[current_index];
    
            let operator_precedence = match operator_token.kind {
                TokenKind::Plus | TokenKind::Minus => 1,
                TokenKind::Times | TokenKind::Divided | TokenKind::Percent => 2,
                _ => 0,
            };
    
            if operator_precedence <= precedence {
                break;
            }
    
            current_index += 1;
    
            if operator_token.kind == TokenKind::LeftParen {
                let closing_index = self.find_matching_parenthesis(&tokens[current_index..]);
                if closing_index.is_none() {
                    return None;
                }
    
                let sub_expression = &tokens[(current_index + 1)..(current_index + closing_index.unwrap())];
                let sub_result = self.parse_math_expression(sub_expression.to_vec());
    
                if sub_result.is_none() {
                    return None;
                }
    
                lhs = match operator_precedence {
                    1 => Math::Addition(Box::new(lhs), Box::new(sub_result.unwrap())),
                    2 => Math::Multiplication(Box::new(lhs), Box::new(sub_result.unwrap())),
                    _ => return None,
                };
    
                current_index += closing_index.unwrap() + 1;
            } else {
                let rhs = match self.parse_math_expression_helper(&tokens[current_index..], operator_precedence) {
                    Some(expr) => expr,
                    None => return None,
                };
    
                lhs = match operator_token.kind {
                    TokenKind::Plus => Math::Addition(Box::new(lhs), Box::new(rhs)),
                    TokenKind::Minus => Math::Subtraction(Box::new(lhs), Box::new(rhs)),
                    TokenKind::Times => Math::Multiplication(Box::new(lhs), Box::new(rhs)),
                    TokenKind::Divided => Math::Division(Box::new(lhs), Box::new(rhs)),
                    TokenKind::Percent => Math::Remainder(Box::new(lhs), Box::new(rhs)),
                    _ => return None,
                };
    
                current_index += 1;
            }
        }
    
        Some(lhs)
    }
    
    fn find_matching_parenthesis(&self, tokens: &[Token]) -> Option<usize> {
        let mut count = 0;
    
        for (index, token) in tokens.iter().enumerate() {
            match token.kind {
                TokenKind::LeftParen => count += 1,
                TokenKind::RightParen => {
                    count -= 1;
                    if count == 0 {
                        return Some(index);
                    }
                }
                _ => continue,
            }
        }
    
        None
    }
}