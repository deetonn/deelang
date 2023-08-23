
#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    // Single-character tokens.
    LeftParen, RightParen, LeftBrace, RightBrace,
    Comma, Dot, Minus, Plus, Semicolon, Slash, Star,
    Percent, Caret, Ampersand, Pipe, Arrow,
  
    // One or two character tokens.
    Bang, BangEqual,
    Equal, EqualEqual,
    Greater, GreaterEqual,
    Less, LessEqual, And, Or,

    ThreeDots, Colon, DoubleColon, QuestionMark, Hashtag,

    PlusEqual, MinusEqual, StarEqual, SlashEqual,
    PercentEqual, CaretEqual, AmpersandEqual, PipeEqual,
  
    // Literals.                      
    Identifier(String), String(String),
    Number(usize), Float(f64),
  
    // Keywords.
    Struct, Enum, Fn, Let, Const, If, Else, Return, 
    True, False, While, For, Break, Continue, MySelf,
    Import,
  
    Eof
}