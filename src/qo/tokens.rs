#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    Keyword(String),
    Identifier(String),
    NumericLiteral(i64),
    FloatLiteral(f64),
    StringLiteral(String),
    BoolLiteral(bool),
    Type(String),
    NoneLiteral,
    Plus,
    Minus,
    Times,
    Divided,
    Percent,
    LogicalAnd,
    LogicalOr,
    LogicalXor,
    LeftShift,
    RightShift,
    EqualTo,
    LessThan,
    GreaterThan,
    LessThanE,
    GreaterThanE,
    Equals,
    PlusEquals,
    MinusEquals,
    TimesEquals,
    DividedEquals,
    PercentEquals,
    LeftParen,
    RightParen,
    LeftBrac,
    RightBrac,
    LeftKey,
    RightKey,
    Dot,
    Comma,
    RuleSet,
    Colon,
    Semicolon,
    Ellipsis,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub length: usize,
    pub line: usize,
    pub col: usize,
}