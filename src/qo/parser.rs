use std::io::Read;

use fxhash::FxHashMap;

use super::{tokens::{Token, TokenKind}, tokenizer::Tokenizer};

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    LetDeclaration(String, Expression),
    Assignment(String, Expression),
    Conditional(Condition, Vec<Statement>),
    Expr(Expression),
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
    List(Vec<Expression>)
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
    pub variable_types_map: FxHashMap<String, QoTypes>,
    pub function_types_map: FxHashMap<String, (Option<Vec<(String, Vec<QoTypes>)>>, QoTypes)>,
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

    pub fn parse(&mut self) -> Result<Vec<Statement>, (Vec<String>, Option<Token>)> {
        let functions = super::function_data::return_function_data();
        self.function_types_map.extend(functions.into_iter());

        loop {
            let next_token = self.iter.next();
            match next_token {
                Some(token) => {
                    match &token.kind {
                        TokenKind::Keyword(k) => {
                            match k.as_str() {
                                "let" => {
                                    let name = self.parse_identifier()?;
                                    let variable_type = self.parse_annotation()?;
                                    self.variable_types_map.insert(name.clone(), variable_type);
                                    self.expect_kind(TokenKind::Equals)?;
                                    let expr = self.evaluate_expected(Some(variable_type), false)?;
                                    self.expect_kind(TokenKind::Semicolon)?;
                                    self.statements.push(Statement::LetDeclaration(name, expr))
                                }
                                "import" => {
                                    let tok = self.iter.next();
                                    match &tok {
                                        Some(Token { kind: TokenKind::StringLiteral(s), .. }) => {
                                            if !std::path::Path::new(&s).exists() {
                                                return Err((vec![format!("The file `{}` does not exist.", s)], tok))
                                            } else {
                                                let file = std::fs::File::open(&s);
                                                match file {
                                                    Ok(mut file) => {
                                                        let mut buf = String::new();
                                                        match file.read_to_string(&mut buf) {
                                                            Ok(_) => {
                                                                let mut tokenizerr = Tokenizer::new(&buf);
                                                                let tokens = tokenizerr.tokenize();
                                                                if let Ok(tokens) = tokens {
                                                                    let mut parser = Parser::new(&tokens);
                                                                    let statements = parser.parse()?;
                                                                    self.statements.extend(statements);
                                                                } else if let Err(err) = tokens {
                                                                    return Err((vec![err], tok))
                                                                }
                                                            }
                                                            Err(err) => {
                                                                return Err((vec![format!("Could not read file `{}`: {}", s, err)], tok))
                                                            }
                                                        }
                                                    }
                                                    Err(err) => {
                                                        return Err((vec![format!("Could not open file `{}`: {}", s, err)], tok))
                                                    }
                                                }
                                            }
                                        }
                                        Some(_) => {
                                            return Err((vec![format!("Expected token `StringLiteral`, found `{:?}`", tok)], tok))
                                        }
                                        None => {
                                            return Err((vec![format!("Unexpected EOF")], Some(token)))
                                        }
                                    }
                                }
                                _ => {
                                    panic!("Exhaustive handling of keywords in Parser.parse: {}", k)
                                }
                            }
                        }
                        TokenKind::Identifier(ident) => {
                            if self.variable_types_map.contains_key(ident) {
                                self.expect_kind(TokenKind::Equals)?;
                                let expected_type = self.variable_types_map.get(ident).unwrap();
                                let expr = self.evaluate_expected(Some(expected_type.clone()), false)?;
                                self.statements.push(Statement::Assignment(ident.clone(), expr));
                            } else if self.function_types_map.contains_key(ident) {
                                let types = self.function_types_map.get(ident).unwrap().clone();
                                let args = self.parse_function_arguments()?;
                                if let Some(types) = types.0 {
                                    for (arg_provided, (_, arg_type)) in args.iter().zip(types.iter()) {
                                        if let Err(err) = self.typecheck(arg_provided, arg_type) {
                                            return Err((vec![err], Some(token)))
                                        }
                                    }
                                }
                                self.expect_kind(TokenKind::Semicolon)?;
                                self.statements.push(Statement::Expr(Expression::FunctionCall(ident.clone(), args)))
                            } else {
                                let ident_as_str = ident.as_str();
                                if ["fn", "func", "function", "def"].contains(&ident_as_str) {
                                    return Err((vec![format!("Unexpected location for unbound identifier `{}`", &ident),
                                    "Use the keyword `fun` to declare functions instead".to_string(),
                                    "fun myfun(arg: i32): i64 {{ /* statements */ }}".to_string()], Some(token)))
                                } else if ["var", "const", "static"].contains(&ident_as_str) {
                                    return Err((vec![format!("Unexpected location for unbound identifier `{}`", &ident),
                                    "Use the keyword `let` to declare variables instead".to_string(),
                                    "let myvar: i32 = 9;".to_string()], Some(token)))
                                } else if ident_as_str == "int" {
                                    return Err((vec![format!("Unexpected location for unbound identifier `{}`", &ident),
                                    "`int` is not a valid type, use the type `i32` for an 32-bit integer instead".to_string(),
                                    "let myint: i32 = 16;".to_string()], Some(token)))
                                } else if ident_as_str == "long" {
                                    return Err((vec![format!("Unexpected location for unbound identifier `{}`", &ident),
                                    "`long` is not a valid type, use the type `i64` for an 64-bit integer instead".to_string(),
                                    "let mylong: i64 = 99999;".to_string()], Some(token)))
                                } else if ident_as_str == "boolean" {
                                    return Err((vec![format!("Unexpected location for unbound identifier `{}`", &ident),
                                    "`boolean` is not a valid type, use the type `bool` for a value that represents true or false".to_string(),
                                    "let mybool: bool = true;".to_string()], Some(token)))
                                } else if ["class", "struct", "impl"].contains(&ident_as_str) {
                                    return Err((vec![format!("Unexpected location for unbound identifier `{}`", &ident),
                                    "Classes are not a valid data structure: use the keyword `interface` instead".to_string(),
                                    "interface MyInferface {\n\tfield1: i32,\n}".to_string()], Some(token)))
                                } else {
                                    return Err((vec![format!("Unexpected location for unbound identifier `{}`", &ident)], Some(token)))
                                }
                            }
                        }
                        _ => {
                            return Err((vec![format!("Unexpected location for token `{:?}`", &token.kind)], Some(token)))
                        }
                    }
                }
                None => break,
            }
        }
        Ok(self.statements.clone())
    }

    pub fn typecheck(&self, expression: &Expression, type_: &[QoTypes]) -> Result<(), String> {
        match (expression, type_) {
            (Expression::Numerical(_), type_) if type_.contains(&QoTypes::I32) => {
                return Ok(())
            }
            (Expression::Numerical(_), type_) if type_.contains(&QoTypes::I64) => {
                return Ok(())
            }
            (Expression::Numerical(_), type_) if type_.contains(&QoTypes::F32) => {
                return Ok(())
            }
            (Expression::Numerical(_), type_) if type_.contains(&QoTypes::F64) => {
                return Ok(())
            }
            (Expression::Numerical(_), type_) if type_.contains(&QoTypes::Any) => {
                return Ok(())
            }
            (Expression::Variable(name), _) => {
                let vartype = self.variable_types_map.get(name);
                match vartype {
                    Some(vartype) => {
                        return self.typecheck(expression, &[*vartype])
                    } 
                    None => {
                        return Err(format!("The variable `{}` is not defined", name))
                    }
                }
            }
            (Expression::Literal(TokenKind::StringLiteral(_)), types) if types.contains(&QoTypes::String) => {
                return Ok(())
            }
            (Expression::Literal(TokenKind::BoolLiteral(_)), types) if types.contains(&QoTypes::Bool) => {
                return Ok(())
            }
            (Expression::Literal(TokenKind::FloatLiteral(_)), types) if types.contains(&QoTypes::F32) => {
                return Ok(())
            }
            (Expression::Literal(TokenKind::FloatLiteral(_)), types) if types.contains(&QoTypes::F64) => {
                return Ok(())
            }
            (Expression::Literal(TokenKind::NumericLiteral(_)), types) if types.contains(&QoTypes::I64) => {
                return Ok(())
            }
            (Expression::Literal(TokenKind::NumericLiteral(_)), types) if types.contains(&QoTypes::I32) => {
                return Ok(())
            }
            (Expression::FunctionCall(name, _), typereturns) if !typereturns.contains(&QoTypes::Any) => {
                if let Some(types) = self.function_types_map.get(name) {
                    if  !type_.contains(&types.1) {
                        return Err(format!("The function `{}` returns a value of type {:?} which is not compatible with the expected type {:?}",
                        name, types.1, type_))
                    } else {
                        return Ok(())
                    }
                } else {
                    return Err(format!("The function `{}` is not defined", name))
                }
            }
            (Expression::FunctionCall(name, _), _) => {
                if !self.function_types_map.contains_key(name) {
                    return Err(format!("The function `{}` is not defined", name))
                } else {
                    return Ok(())
                }
            }
            _ => {
                return Err(format!("The expression `{:?}` is not compatible with a type of `{:?}`", expression, type_))
            }
        }
    }

    pub fn parse_identifier(&mut self) -> Result<String, (Vec<String>, Option<Token>)> {
        match self.iter.next() {
            Some(token) => {
                match &token.kind {
                    TokenKind::Identifier(i) => {
                        return Ok(i.to_string())
                    }
                    _ => {
                        return Err((vec![format!("Expected an identifier here, found: {:?}", &token)], Some(token)))
                    }
                }
            }
            None => {
                return Err((vec![format!("Expected an identifier here, but found EOF")], None))
            }
        }
    }

    pub fn expect_kind(&mut self, kind: TokenKind) -> Result<(), (Vec<String>, Option<Token>)> {
        match self.iter.next() {
            Some(token) => {
                if &token.kind != &kind {
                    return Err((vec![format!("Expected token `{:?}`, found `{:?}`", kind, &token.kind)], Some(token)))
                } else {
                    Ok(())
                }
                
            }
            None => {
                return Err((vec![format!("Expected a `{:?}` here, found EOF", kind)], None))
            }
        }
    }

    pub fn expect_kind_returned(&mut self, kind: TokenKind) -> Result<TokenKind, (Vec<String>, Option<Token>)> {
        match self.iter.next() {
            Some(token) => {
                if &token.kind != &kind {
                    return Err((vec![format!("Expected token `{:?}`, found `{:?}`", kind, &token.kind)], Some(token)))
                } else {
                    Ok(token.kind)
                }
            }
            None => {
                return Err((vec![format!("Expected an identifier here, found EOF")], None))
            }
        }
    }

    pub fn evaluate_expected(&mut self, expected: Option<QoTypes>, fun_should_skip: bool) -> Result<Expression, (Vec<String>, Option<Token>)> {
        let peeked = self.iter.next();
        eprintln!("ran evaluate_expected");
        match peeked {
            Some(token) => {
                match &token.kind {
                    TokenKind::NumericLiteral(_) => {
                        if let Some(expected) = expected {
                            if expected != QoTypes::I32 && expected != QoTypes::I64 {
                                return Err((vec![format!("A numeric literal is not compatible with the type {:?}", expected)], Some(token)))
                            }
                        }
                        let mut tokens = self.get_while_not_valid();
                        tokens.insert(0, token.clone());
                        let expr = self.parse_math_expression(tokens);
                        match expr {
                            Some(expr) => {
                                Ok(Expression::Numerical(expr))
                            }
                            None => {
                                return Err((vec![format!("Error during math expression parsing")], Some(token)))
                            }
                        }
                    }
                    TokenKind::StringLiteral(_) => {
                        if let Some(expected) = expected {
                            if expected != QoTypes::String {
                                return Err((vec![format!("A string literal is not compatible with the type {:?}", expected)], Some(token)))
                            }
                        }
                        Ok(Expression::Literal(token.kind))
                    } 
                    TokenKind::FloatLiteral(f) => {
                        if let Some(expected) = expected {
                            if expected != QoTypes::F32 && expected != QoTypes::F64 {
                                return Err((vec![format!("A float literal is not compatible with the type {:?}", expected)], Some(token)))
                            }
                        }
                        let mut tokens = self.get_while_not_valid();
                        tokens.insert(0, token.clone());
                        let expr = self.parse_math_expression(tokens);
                        match expr {
                            Some(expr) => {
                                Ok(Expression::Numerical(expr))
                            }
                            None => {
                                return Err((vec![format!("Error during math expression parsing")], Some(token)))
                            }
                        }
                    }
                    TokenKind::BoolLiteral(_) => {
                        if let Some(expected) = expected {
                            if expected != QoTypes::Bool {
                                return Err((vec![format!("A bool literal is not compatible with the type {:?}", expected)], Some(token)))
                            }
                        }
                        Ok(Expression::Literal(token.kind))
                    }
                    TokenKind::NoneLiteral => {
                        if let Some(expected) = expected {
                            if expected != QoTypes::Bool {
                                return Err((vec![format!("A None literal is not compatible with the type {:?}", expected)], Some(token)))
                            }
                        }
                        Ok(Expression::Literal(token.kind))
                    }
                    TokenKind::Identifier(i) => {
                        if self.variable_types_map.contains_key(i) {
                            eprintln!("Somehow got here");
                            eprintln!("Current statements: {:?}", self.statements);
                            let type_ = self.variable_types_map.get(i).unwrap().clone();
                            if let Some(expected) = expected {
                                if type_ != expected {
                                    return Err((vec![format!("The variable `{}` has a type of {:?} that is not assigneable to a variable of type {:?}",
                                    i, type_, expected)], Some(token)));
                                } else {
                                    Ok(Expression::Variable(i.clone()))
                                }
                            } else {
                                Ok(Expression::Variable(i.clone()))
                            }
                        } else if self.function_types_map.contains_key(i) {
                            eprintln!("function parser attempt");
                            let types = self.function_types_map.get(i).unwrap().clone();
                            if let Some(expected) = expected {
                                if expected != types.1 {
                                    return Err((vec![format!("The function `{}` returns a value of type `{:?}` which is not compatible with a type of `{:?}`",
                                    i, types.1, expected)], Some(token)))
                                }
                            }
                            let args = self.parse_function_arguments()?;
                            if let Some(types) = types.0 {
                                for (arg_provided, (_, arg_type)) in args.iter().zip(types.iter()) {
                                    if let Err(err) = self.typecheck(arg_provided, arg_type) {
                                        return Err((vec![err], Some(token)))
                                    }
                                }
                            }
                            Ok(Expression::FunctionCall(i.clone(), args))
                        } else {
                            return Err((vec![format!("Unexpected token `{:?}`", &token.kind)], Some(token)))
                        }
                    }
                    TokenKind::LeftBrac => { // If the token is a left bracket, it starts a list
                        // Create an empty vector to store the list elements
                        let mut list_elements: Vec<Expression> = Vec::new();
        
                        eprintln!("overflow is here!");

                        // Loop until we find a right bracket or an error
                        loop {
        
                            // Get the next token from the iterator
                            let next_token = self.iter.peek();
        
                            // Check what kind of token it is
                            match next_token {
        
                                // If it is a right bracket, we end the list
                                Some(Token { kind: TokenKind::RightBrac, .. }) => {
                                    self.iter.next();
                                    break;
                                }
        
                                // If it is a comma, we skip it
                                Some(Token { kind: TokenKind::Comma, .. }) => {
                                    self.iter.next();
                                    continue;
                                }
        
                                // If it is anything else, we try to evaluate it as an expression
                                Some(_) => {
        
                                    // Call the evaluate_expected function recursively with no expected type
                                    let element = self.evaluate_expected(None, false)?;
        
                                    // Push the element into the list vector
                                    list_elements.push(element);
                                }
        
                                // If there is no token, we return an error
                                None => {
                                    return Err((vec![format!("Expected a right bracket here, found EOF")], None));
                                }
                            }
                        }

                        // Return the list expression
                        Ok(Expression::List(list_elements))
                    }
                    _ => {
                        return Err((vec![format!("Expected an expression, found `{:?}`", &token.kind)], Some(token)))
                    }
                }
            }
            None => {
                return Err((vec![format!("Expected an expression here, found EOF")], None))
            }
        }
    }

    pub fn parse_annotation(&mut self) -> Result<QoTypes, (Vec<String>, Option<Token>)> {
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
                                        return Err((vec![
                                            format!("Expected token `Type`, found `{:?}`", &token.kind)
                                        ], Some(token)))
                                    }
                                }
                            }
                            None => {
                                return Err((vec![format!("Expected a colon here")], None))
                            }
                        }
                    }
                    _ => {
                        return Err((vec![format!("Expected token `Colon`, found `{:?}`", &token.kind)], Some(token)))
                    }
                }
            }
            None => {
                return Err((vec![format!("Expected a colon here")], None))
            }
        }
    }

    pub fn parse_function_arguments(&mut self) -> Result<Vec<Expression>, (Vec<String>, Option<Token>)> {
        self.expect_kind(TokenKind::LeftParen)?;
        let mut expressions = Vec::new();
     
        loop {
            if let Some(Token { kind: TokenKind::RightParen, .. }) = self.iter.peek() {
                self.iter.next();
                break;
            }
            eprintln!("{:?}", expressions);
            let expr = self.evaluate_expected(None, true)?;
            expressions.push(expr);
            if let Some(Token { kind: TokenKind::Comma, .. }) = self.iter.peek() {
                self.iter.next();
            }
        }
        Ok(expressions)
    }

    pub fn get_while_not_valid(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        let mut paren_count = 0;
        while let Some(token) = self.iter.peek() {
            match &token.kind {
                TokenKind::Semicolon => {
                    break;
                }
                TokenKind::RightParen => {
                    paren_count -= 1;
                    if paren_count < 0 {
                        break;
                    }
                }
                TokenKind::LeftParen => {
                    paren_count += 1;
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

pub fn format_error(source: &str, message: &str, token: Token) -> String {
    // Use ansi_term crate to create red carets
    use ansi_term::Colour::Red;
    let carets = Red.paint("^".repeat(token.length));

    // Split the source code by lines and get the line where the error occurred
    let lines: Vec<&str> = source.split('\n').collect();
    let error_line = lines[token.line - 1];

    // Create a string with the error message, the line number, the source code and the carets
    let mut output = String::new();
    output.push_str(message);
    output.push('\n');
    output.push_str(&format!("{} | {}\n", token.line, error_line));
    // Add spaces to align the carets with the column
    output.push_str(&" ".repeat(token.line.to_string().len() + 3 + token.col - 1));
    // Add the carets
    output.push_str(&carets.to_string());
    output
}