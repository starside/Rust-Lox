use lox_derive::EnumStrings;

pub trait enum_vectorize {
    fn enum_to_vector(&self) -> Vec<String>;
}

trait enum_element {
    fn enum_to_element(&self) -> String;
}

pub struct TokenMetadata {
    pub line: usize
}

pub struct TokenTextValueMetadata<'a> {
    pub(crate) metadata: TokenMetadata,
    pub(crate) lexeme: &'a str
}

pub struct TokenNumberValueMetadata {
    metadata: TokenMetadata,
    value: f64
}

impl enum_element for TokenMetadata {
    fn enum_to_element(&self) -> String {
        self.line.to_string()
    }
}

impl enum_element for TokenTextValueMetadata<'_> {
    fn enum_to_element(&self) -> String {
        self.lexeme.to_string()
    }

}

impl enum_element for TokenNumberValueMetadata {
    fn enum_to_element(&self) -> String {
        self.value.to_string()
    }
}

#[derive(EnumStrings)]
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
