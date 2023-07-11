use super::tokens::{Token, TokenKind};

#[derive(Clone, Debug, PartialEq)]
pub struct Tokenizer {
    input:  String,
    line:   usize,
    column: usize,
    tokens: Vec<Token>,
}

impl Tokenizer {
    pub fn new<T: ToString>(input: T) -> Tokenizer {
        return Self {
            input: input.to_string().replace("\r\n", "\n"),
            line: 1,
            column: 1,
            tokens: Vec::new(),
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, String> {
        let mut iter = self.input.chars().peekable();
        loop {
            let next_char = iter.next();
            match next_char {
                Some(chr) => {
                    match chr {
                        ':' => {
                            self.tokens.push(Token {
                                kind: TokenKind::Colon,
                                length: 1,
                                line: self.line,
                                col: self.column,
                            });
                            self.column += 1;
                        }
                        ';' => {
                            self.tokens.push(Token {
                                kind: TokenKind::Semicolon,
                                length: 1,
                                line: self.line,
                                col: self.column,
                            });
                            self.column += 1;
                        }
                        'a'..='z' | 'A'..='Z' | '_' => {
                            let mut identifier = String::new();
                            identifier.push(chr);
                            while let Some(chr) = iter.peek() {
                                if chr.is_ascii_alphanumeric() || chr == &'.' || chr == &'_' {
                                    identifier.push(*chr);
                                    iter.next();
                                } else {
                                    break;
                                }
                            }
                            if ["let", "import", "fun", "return", "typeof"].contains(&identifier.as_str()) {
                                let length = identifier.len();
                                self.tokens.push(Token {
                                    kind: TokenKind::Keyword(identifier),
                                    length,
                                    line: self.line,
                                    col: self.column,
                                });
                                self.column += length;
                            } else if ["true", "false"].contains(&identifier.as_str()) {
                                let length = identifier.len();
                                self.tokens.push(Token {
                                    kind: TokenKind::BoolLiteral(&identifier != "false"),
                                    length,
                                    line: self.line,
                                    col: self.column,
                                });
                                self.column += length;
                            } else if &identifier == "None" {
                                self.tokens.push(Token {
                                    kind: TokenKind::NoneLiteral,
                                    length: 4,
                                    line: self.line,
                                    col: self.column,
                                });
                                self.column += 4;
                            } else if ["i32", "i64", "f32", "f64", "string",
                                       "char", "bool", "list", "tuple", "uninit",
                                       "Error", "pointer", "File", "bytes", "Future", "any"].contains(&identifier.as_str()) {
                                self.tokens.push(Token {
                                    kind: TokenKind::Type(identifier.clone()),
                                    length: identifier.len(),
                                    line: self.line,
                                    col: self.column,
                                });
                                self.column += identifier.len();
                            } else {
                                let length = identifier.len();
                                self.tokens.push(Token {
                                    kind: TokenKind::Identifier(identifier),
                                    length,
                                    line: self.line,
                                    col: self.column,
                                });
                                self.column += length;
                            }
                        }
                        '0'..='9' => {
                            let mut number = String::new();
                            let mut underscores_length = 0;
                            number.push(chr);
                            while let Some(chr) = iter.peek() {
                                if chr.is_numeric() || chr == &'.' {
                                    number.push(*chr);
                                    iter.next();
                                } else if chr == &'_' {
                                    underscores_length += 1;
                                    iter.next();
                                    continue;
                                } else {
                                    break;
                                }
                            }
                            if number.contains('.') {
                                let parsing_result = number.parse::<f64>();
                                match parsing_result {
                                    Ok(parsed_number) => {
                                        self.tokens.push(Token {
                                            kind: TokenKind::FloatLiteral(parsed_number),
                                            length: number.len() + underscores_length,
                                            line: self.line,
                                            col: self.column,
                                        });
                                        self.column += number.len() + underscores_length;
                                    }
                                    Err(err) => {
                                        return Err(format!("{}:{}: Error parsing float literal: {}", self.line, self.column, err))
                                    }
                                }
                            } else {
                                let parsing_result = number.parse::<i64>();
                                match parsing_result {
                                    Ok(parsed_number) => {
                                        self.tokens.push(Token {
                                            kind: TokenKind::NumericLiteral(parsed_number),
                                            length: number.len() + underscores_length,
                                            line: self.line,
                                            col: self.column,
                                        });
                                        self.column += number.len() + underscores_length;
                                    }
                                    Err(err) => {
                                        return Err(format!("{}:{}: Error parsing integer literal: {}", self.line, self.column, err))
                                    }
                                }
                            }
                        }
                        '\"' => {
                            let mut reached = false;
                            let mut string = String::new();
                            while let Some(ch) = iter.next() {
                                match ch {
                                    '\"' => {
                                        reached = true;
                                        break;
                                    }
                                    '\n' => {
                                        self.line += 1;
                                        self.column = 1;
                                        string.push('\n');
                                    }
                                    '\r' => {
                                        if Some(&'\n') == iter.peek() {
                                            self.line += 1;
                                            self.column = 1;
                                            string.push_str("\r\n");
                                            iter.next();
                                        } else {
                                            string.push('\r');
                                        }
                                    }
                                    '\\' => {
                                        match iter.next() {
                                            Some(c) => {
                                                match c {
                                                    'n' => {
                                                        string.push('\n');
                                                    }
                                                    'r' => {
                                                        string.push('\r');
                                                    }
                                                    't' => {
                                                        string.push('\t');
                                                    }
                                                    '\\' => {
                                                        string.push('\\');
                                                    }
                                                    '0' => {
                                                        string.push('\0');
                                                    }
                                                    'u' => {
                                                        let mut digits = String::new();
                                                        for _ in 0..4 {
                                                            match iter.next() {
                                                                Some(digit) => {
                                                                    digits.push(digit);
                                                                }
                                                                None => {
                                                                    return Err(format!(
                                                                        "{}:{}: A unicode escape sequence must have 4 hexadecimal digits in the sense of \\u{{7FFF}}",
                                                                        self.line, self.column
                                                                    ))
                                                                }
                                                            }
                                                        }
                                                        if let Ok(num) = u32::from_str_radix(&digits, 16) {
                                                            if let Some(ch) = char::from_u32(num) {
                                                                string.push(ch);
                                                            }
                                                        } else if let Err(err) =
                                                            u32::from_str_radix(&digits, 16)
                                                        {
                                                            return Err(format!(
                                                                "{}:{}: Error during unicode escape sequence '\\u{}' parsing: {}",
                                                                self.line, self.column, digits, err
                                                            ));
                                                        }
                                                    }
                                                    '"' => {
                                                        string.push('"');
                                                    }
                                                    _ => {
                                                        return Err(format!(
                                                            "{}:{}: Unknown escape sequence '\\{}'",
                                                            self.line, self.column, c
                                                        ))
                                                    }
                                                }
                                            }
                                            None => {
                                                return Err(format!(
                                                    "{}:{}: Unclosed string literal",
                                                    self.line, self.column
                                                ));
                                            }
                                        }
                                    }
                                    _ => {
                                        string.push(ch);
                                    }
                                }
                            }
                            if !reached {
                                return Err(format!(
                                    "{}:{}: Unclosed string literal",
                                    self.line, self.column
                                ));
                            }
                            let strlen = string.len();
                            self.tokens.push(Token {
                                kind: TokenKind::StringLiteral(string),
                                length: strlen + 2,
                                line: self.line,
                                col: self.column,
                            });
                            self.column += strlen + 2;
                        }
                        '+' => {
                            if let Some(&'=') = iter.peek() {
                                iter.next();
                                self.tokens.push(Token {
                                    kind: TokenKind::PlusEquals,
                                    length: 2,
                                    line: self.line,
                                    col: self.column,
                                });
                                self.column += 2;
                            } else {
                                self.tokens.push(Token {
                                    kind: TokenKind::Plus,
                                    length: 1,
                                    line: self.line,
                                    col: self.column,
                                });
                                self.column += 1;
                            }
                        }
                        '-' => {
                            if let Some(&'=') = iter.peek() {
                                iter.next();
                                self.tokens.push(Token {
                                    kind: TokenKind::MinusEquals,
                                    length: 2,
                                    line: self.line,
                                    col: self.column,
                                });
                                self.column += 2;
                            } else {
                                self.tokens.push(Token {
                                    kind: TokenKind::Minus,
                                    length: 1,
                                    line: self.line,
                                    col: self.column,
                                });
                                self.column += 1;
                            }
                        }
                        '*' => {
                            if let Some(&'=') = iter.peek() {
                                iter.next();
                                self.tokens.push(Token {
                                    kind: TokenKind::TimesEquals,
                                    length: 2,
                                    line: self.line,
                                    col: self.column,
                                });
                                self.column += 2;
                            } else {
                                self.tokens.push(Token {
                                    kind: TokenKind::Times,
                                    length: 1,
                                    line: self.line,
                                    col: self.column,
                                });
                                self.column += 1;
                            }
                        }
                        '/' => {
                            if let Some(&'=') = iter.peek() {
                                iter.next();
                                self.tokens.push(Token {
                                    kind: TokenKind::DividedEquals,
                                    length: 2,
                                    line: self.line,
                                    col: self.column,
                                });
                                self.column += 2;
                            } else {
                                self.tokens.push(Token {
                                    kind: TokenKind::Divided,
                                    length: 1,
                                    line: self.line,
                                    col: self.column,
                                });
                                self.column += 1;
                            }
                        }
                        '%' => {
                            if let Some(&'=') = iter.peek() {
                                iter.next();
                                self.tokens.push(Token {
                                    kind: TokenKind::PercentEquals,
                                    length: 2,
                                    line: self.line,
                                    col: self.column,
                                });
                                self.column += 2;
                            } else {
                                self.tokens.push(Token {
                                    kind: TokenKind::Percent,
                                    length: 1,
                                    line: self.line,
                                    col: self.column,
                                });
                                self.column += 1;
                            }
                        }
                        '&' if iter.peek() == Some(&'&') => {
                            iter.next();
                            self.tokens.push(Token {
                                kind: TokenKind::LogicalAnd,
                                length: 2,
                                line: self.line,
                                col: self.column,
                            });
                            self.column += 2;
                        }
                        '|' if iter.peek() == Some(&'|') => {
                            iter.next();
                            self.tokens.push(Token {
                                kind: TokenKind::LogicalOr,
                                length: 2,
                                line: self.line,
                                col: self.column,
                            });
                            self.column += 2;
                        }
                        '^' => {
                            self.tokens.push(Token {
                                kind: TokenKind::LogicalXor,
                                length: 1,
                                line: self.line,
                                col: self.column,
                            });
                            self.column += 1;
                        }
                        '<' if iter.peek() == Some(&'<') => {
                            iter.next();
                            self.tokens.push(Token {
                                kind: TokenKind::LeftShift,
                                length: 2,
                                line: self.line,
                                col: self.column,
                            });
                            self.column += 2;
                        }
                        '>' if iter.peek() == Some(&'>') => {
                            iter.next();
                            self.tokens.push(Token {
                                kind: TokenKind::RightShift,
                                length: 2,
                                line: self.line,
                                col: self.column,
                            });
                            self.column += 2;
                        }
                        '<' if iter.peek() == Some(&'=') => {
                            iter.next();
                            self.tokens.push(Token {
                                kind: TokenKind::LessThanE,
                                length: 2,
                                line: self.line,
                                col: self.column,
                            });
                            self.column += 2;
                        }
                        '>' if iter.peek() == Some(&'=') => {
                            iter.next();
                            self.tokens.push(Token {
                                kind: TokenKind::GreaterThanE,
                                length: 2,
                                line: self.line,
                                col: self.column,
                            });
                            self.column += 2;
                        }
                        '<' => {
                            self.tokens.push(Token {
                                kind: TokenKind::LessThan,
                                length: 1,
                                line: self.line,
                                col: self.column,
                            });
                            self.column += 1;
                        }
                        '>' => {
                            self.tokens.push(Token {
                                kind: TokenKind::GreaterThan,
                                length: 1,
                                line: self.line,
                                col: self.column,
                            });
                            self.column += 1;
                        }
                        '(' => {
                            self.tokens.push(Token {
                                kind: TokenKind::LeftParen,
                                length: 1,
                                line: self.line,
                                col: self.column,
                            });
                            self.column += 1;
                        }
                        ')' => {
                            self.tokens.push(Token {
                                kind: TokenKind::RightParen,
                                length: 1,
                                line: self.line,
                                col: self.column,
                            });
                            self.column += 1;
                        }
                        '[' => {
                            self.tokens.push(Token {
                                kind: TokenKind::LeftBrac,
                                length: 1,
                                line: self.line,
                                col: self.column,
                            });
                            self.column += 1;
                        }
                        ']' => {
                            self.tokens.push(Token {
                                kind: TokenKind::RightBrac,
                                length: 1,
                                line: self.line,
                                col: self.column,
                            });
                            self.column += 1;
                        }
                        '{' => {
                            self.tokens.push(Token {
                                kind: TokenKind::LeftKey,
                                length: 1,
                                line: self.line,
                                col: self.column,
                            });
                            self.column += 1;
                        }
                        '}' => {
                            self.tokens.push(Token {
                                kind: TokenKind::RightKey,
                                length: 1,
                                line: self.line,
                                col: self.column,
                            });
                            self.column += 1;
                        }
                        '.' => {
                            if let Some(&'.') = iter.peek() {
                                iter.next();
                                if let Some(&'.') = iter.peek() {
                                    iter.next();
                                    self.tokens.push(Token {
                                        kind: TokenKind::Ellipsis,
                                        length: 3,
                                        line: self.line,
                                        col: self.column,
                                    });
                                    self.column += 3;
                                    continue;
                                } else {
                                    return Err(format!("{}:{}: Unrecognized token '..'", self.line, self.column))
                                }
                            }
                            self.tokens.push(Token {
                                kind: TokenKind::Dot,
                                length: 1,
                                line: self.line,
                                col: self.column,
                            });
                            self.column += 1;
                        }
                        ',' => {
                            self.tokens.push(Token {
                                kind: TokenKind::Comma,
                                length: 1,
                                line: self.line,
                                col: self.column,
                            });
                            self.column += 1;
                        }
                        '#' if iter.peek() == Some(&'!') => {
                            iter.next();
                            self.tokens.push(Token {
                                kind: TokenKind::RuleSet,
                                length: 2,
                                line: self.line,
                                col: self.column,
                            });
                            self.column += 2;
                        }
                        '=' if iter.peek() == Some(&'=') => {
                            iter.next();
                            self.tokens.push(Token {
                                kind: TokenKind::EqualTo,
                                length: 2,
                                line: self.line,
                                col: self.column,
                            });
                            self.column += 2;
                        }
                        '=' => {
                            self.tokens.push(Token {
                                kind: TokenKind::Equals,
                                length: 1,
                                line: self.line,
                                col: self.column,
                            });
                            self.column += 1;
                        }
                        '\n' => {
                            self.line += 1;
                            self.column = 1;
                        }
                        chr if chr.is_whitespace() => {
                            self.column += 1;
                            continue;
                        }
                        _ => {
                            return Err(format!("{}:{}: Unrecognized character '{}'", self.line, self.column, chr))
                        }
                    }
                } 
                None => {
                    break;
                }
            }
        }
        Ok(self.tokens.clone())
    }
}