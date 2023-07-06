#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    Keyword(String),
    Identifier(String),
    Type(String),
    Int(i32),
    Float(f64),
    String(String),
    Boolean(bool),
    Char(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub length: usize,
    pub line: usize,
    pub column: usize,
}

pub fn tokenize(input: &str, filename: &str) -> Result<Vec<Token>, String> {
    let mut iterator = input.chars().peekable();
    let mut tokens_stream: Vec<Token> = Vec::new();
    let mut line = 1;
    let mut column = 0;
    loop {
        match iterator.next() {
            Some(character) => {
                match character {
                    'a'..='z' | 'A'..='Z' => {
                        let mut identifier = String::new();
                        identifier.push(character);
                        while let Some(c) = iterator.next() {
                            if c.is_whitespace() {
                                break;
                            } else if c.is_alphanumeric() || c == '_' {
                                identifier.push(c);
                            } else {
                                break;
                            }
                        }
                        let identifier_len = identifier.len();
                        column += identifier_len;
                        if ["decl", "readln", "putln", "fls", "puterr",
                        "fls", "puterr", "flserr", "assgn", "assgntop", "throw", "push",
                        "pop", "ld", "jmp", "label", "jmps", "add", "sub", "mul", "div",
                        "mod", "pow", "and", "or", "xor", "not", "eq", "ne", "gt", "lt",
                        "gte", "lte", "cth", "ecth", "istd", "incl", "ivk", "tastk", "ret",
                        "getcp", "ivkcm", "setcp", "chasp", "chassm", "hlt", "fdef", "fendef",
                        "dup", "lst", "lnd", "inln", "dbgpstk", "escp", "lscp", "clct", "memrvol", "memwvol",
                        "entscp", "lvvscp", "clct", "asref", "hrefsm", "refdlc", "tpof", "iiof",
                        "grfh", "gwfh", "cfh", "pfhp", "rffh", "rfhts", "rfhtb", "wstfh", "wbtfh",
                        "sv", "rsv", "grfhs", "gwfhs", "cfhs", "rffhs", "pfhps", "alatl", "dfcor",
                        "ecor", "rcor", "awtcorfs", "throwstk"
                        ].contains(&identifier.as_str()) {
                            tokens_stream.push(Token {
                                token_type: TokenType::Keyword(identifier),
                                length: identifier_len,
                                line,
                                column,
                            })
                        } else if ["true", "false"].contains(&identifier.as_str()) {
                            tokens_stream.push(Token {
                                token_type: TokenType::Boolean(&identifier == "true"),
                                length: identifier_len,
                                line,
                                column,
                            })
                        } else if ["None", "class", "int", "bigint", "float", "lfloat", "string",
                        "character", "boolean", "list", "tuple", "uninitialized", "Error", "ptrwrapper",
                        "any"].contains(&identifier.as_str()) {
                            tokens_stream.push(Token {
                                token_type: TokenType::Type(identifier),
                                length: identifier_len,
                                line,
                                column,
                            })
                        } else if identifier.as_str() == "plch" {
                            continue;
                        } else {
                            tokens_stream.push(Token {
                                token_type: TokenType::Identifier(identifier),
                                length: identifier_len,
                                line,
                                column,
                            })
                        }
                    }
                    // Inside the 'match character' block:
                    '0'..='9' => {
                        let mut number = String::new();
                        number.push(character);
                        while let Some(c) = iterator.next() {
                            if c.is_numeric() {
                                number.push(c);
                            } else {
                                break;
                            }
                        }
                        if number.contains('.') {
                            let parsed_number = number.parse::<f64>();
                            if let Ok(num) = parsed_number {
                                let identifier_len = number.len();
                                tokens_stream.push(Token {
                                    token_type: TokenType::Float(num),
                                    length: identifier_len,
                                    line,
                                    column,
                                });
                                column += identifier_len;
                            } else if let Err(err) = parsed_number {
                                return Err(format!("{}:{}:{}: Error parsing float number: {}", filename, line, column, err));
                            }
                        } else {
                            let parsed_number = number.parse::<i32>();
                            if let Ok(num) = parsed_number {
                                let identifier_len = number.len();
                                tokens_stream.push(Token {
                                    token_type: TokenType::Int(num),
                                    length: identifier_len,
                                    line,
                                    column,
                                });
                                column += identifier_len; 
                            } else if let Err(err) = parsed_number {
                                return Err(format!("{}:{}:{}: Error parsing int number: {}", filename, line, column, err));
                            }
                        }
                    }

                    '\"' => {
                        let mut reached = false;
                        let mut string = String::new();
                        while let Some(ch) = iterator.next() {
                            match ch {
                                '\"' => {
                                    reached = true;
                                    break;
                                }
                                '\n' => {
                                    line += 1;
                                    string.push('\n');
                                }
                                '\r' => {
                                    if Some(&'\n') == iterator.peek() {
                                        line += 1;
                                        string.push_str("\r\n");
                                        iterator.next();
                                    } else {
                                        string.push('\r');
                                    }
                                }
                                '\\' => {
                                    match iterator.next() {
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
                                                        match iterator.next() {
                                                            Some(digit) => {
                                                                digits.push(digit);
                                                            }
                                                            None => {
                                                                return Err(format!(
                                                                    "{}:{}: A unicode escape sequence must have 4 hexadecimal digits in the sense of \\u{{7FFF}}",
                                                                    line, column
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
                                                            line, column, digits, err
                                                        ));
                                                    }
                                                }
                                                '"' => {
                                                    string.push('"');
                                                }
                                                _ => {
                                                    return Err(format!(
                                                        "{}:{}: Unknown escape sequence '\\{}'",
                                                        line, column, c
                                                    ))
                                                }
                                            }
                                        }
                                        None => {
                                            return Err(format!(
                                                "{}:{}: Unclosed string literal",
                                                line, column
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
                                line, column
                            ));
                        }
                        let strlen = string.len();
                        tokens_stream.push(Token {
                            token_type: TokenType::String(string),
                            length: strlen + 2,
                            line,
                            column,
                        });
                    }
                    '\'' => {
                        let mut inner_string = String::new();
                        if let Some(chr) = iterator.next() {
                            if chr != '\'' && chr != '\\' {
                                inner_string.push(chr);
                            } else if chr == '\\' {
                                match iterator.next() {
                                    Some(c) => {
                                        match c {
                                            'n' => {
                                                inner_string.push('\n');
                                            }
                                            'r' => {
                                                inner_string.push('\r');
                                            }
                                            't' => {
                                                inner_string.push('\t');
                                            }
                                            '\\' => {
                                                inner_string.push('\\');
                                            }
                                            '0' => {
                                                inner_string.push('\0');
                                            }
                                            'u' => {
                                                let mut digits = String::new();
                                                for _ in 0..4 {
                                                    match iterator.next() {
                                                        Some(digit) => {
                                                            digits.push(digit);
                                                        }
                                                        None => {
                                                            return Err(format!(
                                                                "{}:{}: A unicode escape sequence must have 4 hexadecimal digits in the sense of \\u{{7FFF}}",
                                                                line, column
                                                            ))
                                                        }
                                                    }
                                                }
                                                if let Ok(num) = u32::from_str_radix(&digits, 16) {
                                                    if let Some(ch) = char::from_u32(num) {
                                                        inner_string.push(ch);
                                                    }
                                                } else if let Err(err) =
                                                    u32::from_str_radix(&digits, 16)
                                                {
                                                    return Err(format!(
                                                        "{}:{}: Error during unicode escape sequence '\\u{}' parsing: {}",
                                                        line, column, digits, err
                                                    ));
                                                }
                                            }
                                            _ => {
                                                return Err(format!(
                                                    "{}:{}: Unknown escape sequence '\\{}'",
                                                    line, column, c
                                                ))
                                            }
                                        }
                                    }
                                    None => {
                                        return Err(format!(
                                            "{}:{}: Unclosed string literal",
                                            line, column
                                        ));
                                    }
                                }
                            } else {
                                return Err(format!("{}:{}:{}: Empty character literals are not allowed", filename, line, column))
                            }
                        } else {
                            return Err(format!("{}:{}:{}: Unclosed character literal", filename, line, column))
                        }
                        let string_len = inner_string.len() + 2;
                        column += string_len;
                        tokens_stream.push(Token {
                            token_type: TokenType::Char(inner_string),
                            length: string_len,
                            line,
                            column,
                        })
                    }
                    '-' => {
                        if Some(&'-') == iterator.peek() {
                            iterator.next();
                            while let Some(c) = iterator.next() {
                                if c == '\n' {
                                    line += 1;
                                    column = 0;
                                    break;
                                } else {
                                    continue;
                                }
                            }
                        } else {
                            return Err(format!("{}:{}:{}: Unrecognized token '-{}'", filename, line, column, iterator.peek().unwrap_or(&'?')))
                        }
                    }
                    '\n' => {
                        column = 0;
                        line += 1;
                    }
                    _ => {
                        if character.is_whitespace() {
                            continue;
                        } else {
                            return Err(format!("{}:{}:{}: Unrecognized token '{}'", filename, line, column, character))
                        }
                    }
                }
            }
            None => break,
        }
    }
    Ok(tokens_stream)
}