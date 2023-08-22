pub mod token_type;
pub mod lexer;

#[derive(Debug, Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug)]
pub struct Token {
    pub token_type: token_type::TokenType,
    pub span: Span,
    pub location: SourceLocation,
}

impl Token {
    pub fn try_as_ident(&self) -> Option<&str> {
        match &self.token_type {
            token_type::TokenType::Identifier(ident) => Some(ident),
            _ => None,
        }
    }
}

impl Token {
    pub fn from_other(token_type: token_type::TokenType, other: &Token) -> Token {
        Token {
            token_type,
            span: other.span.clone(),
            location: other.location.clone(),
        }
    }
}

impl Default for Token {
    fn default() -> Self {
        Token {
            token_type: token_type::TokenType::Eof,
            span: Span {
                start: 0,
                end: 0,
            },
            location: SourceLocation {
                file: String::new(),
                line: 0,
                column: 0,
            },
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SourceLocation {
    pub file: String,
    pub line: usize,
    pub column: usize,
}

impl Default for SourceLocation {
    fn default() -> Self {
        SourceLocation {
            file: "<unknown>".to_owned(),
            line: 0,
            column: 0,
        }
    }
}