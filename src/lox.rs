pub struct TokenMetadata {
    pub line: usize
}

struct TokenTextValueMetadata<'a> {
    metadata: TokenMetadata,
    lexeme: &'a str
}

struct TokenNumberValueMetadata {
    metadata: TokenMetadata,
    value: f64
}

pub enum Token<'a> {
    // Single-character tokens
    LeftParen(TokenMetadata),
    RightParen(TokenMetadata),
    LeftBrace(TokenMetadata),
    RightBrace(TokenMetadata),
    Comma(TokenMetadata),
    Dot(TokenMetadata),
    Minus(TokenMetadata),
    Plus(TokenMetadata),
    Semicolon(TokenMetadata),
    Slash(TokenMetadata),
    Star(TokenMetadata),

    // One or two character tokens
    Bang(TokenMetadata),
    BangEqual(TokenMetadata),
    Greater(TokenMetadata),
    GreaterEqual(TokenMetadata),
    Less(TokenMetadata),
    LessEqual(TokenMetadata),

    // Literals
    Identifier(TokenTextValueMetadata<'a>),
    String(TokenTextValueMetadata<'a>),
    Number(TokenNumberValueMetadata),

    // Keywords
    And(TokenMetadata),
    Class(TokenMetadata),
    Else(TokenMetadata),
    False(TokenMetadata),
    Fun(TokenMetadata),
    For(TokenMetadata),
    If(TokenMetadata),
    Nil(TokenMetadata),
    Or(TokenMetadata),
    Print(TokenMetadata),
    Return(TokenMetadata),
    Super(TokenMetadata),
    This(TokenMetadata),
    True(TokenMetadata),
    Var(TokenMetadata),
    While(TokenMetadata),

    Eof,
}

impl Token<'_> {
    pub fn print_token_name(&self) -> String {
        match self {
            Token::LeftParen(_) => String::from("Left Paren"),
            _ => String::from("Other")
        }
    }
}