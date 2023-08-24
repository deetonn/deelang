
use crate::tokenizer::token_type::TokenType;
use crate::tokenizer::{Span, Token, SourceLocation};

pub const KEYWORDS: [(&str, TokenType); 18] = [
    ("struct", TokenType::Struct),
    ("enum", TokenType::Enum),
    ("fn", TokenType::Fn),
    ("let", TokenType::Let),
    ("const", TokenType::Const),
    ("if", TokenType::If),
    ("else", TokenType::Else),
    ("return", TokenType::Return),
    ("true", TokenType::True),
    ("false", TokenType::False),
    ("while", TokenType::While),
    ("for", TokenType::For),
    ("break", TokenType::Break),
    ("continue", TokenType::Continue),
    ("self", TokenType::MySelf),
    ("import", TokenType::Import),
    ("use", TokenType::Use),
    ("trait", TokenType::Trait),
];

pub struct Lexer {
    pub source: String,
    pub tokens: Vec<Token>,
    pub location: SourceLocation,
    position: usize,
    start_position: usize,
}

const EQUALS: char = '=';
const BANG: char = '!';
const LCURLY: char = '{';
const RCURLY: char = '}';

const LBRAC: char = '(';
const RBRAC: char = ')';

const CROC_OPEN: char = '<';
const CROC_CLOSE: char = '>';

const ADD: char = '+';
const SUB: char = '-';
const MUL: char = '*';
const DIV: char = '/';
const MOD: char = '%';

const AND: char = '&';
const OR: char = '|';
const XOR: char = '^';
const NOT: char = '~';

const DOT: char = '.';
const COMMA: char = ',';
const COLON: char = ':';
const SEMI_COLON: char = ';';

impl Lexer {
    pub fn new(source: String, file: Option<String>) -> Lexer {
        Lexer {
            source,
            tokens: Vec::new(),
            location: SourceLocation {
                file: file.unwrap_or("main.dl".to_string()),
                line: 0,
                column: 0,
            },
            position: 0,
            start_position: 0,
        }
    }

    pub fn lex(&mut self) {
        while self.position < self.source.len() {
            self.location.column += 1;
            let c = match self.current() {
                Some(c) => c,
                None => {
                    self.fail("unexpected end of file");
                    return;
                }
            };

            let token = match c {
                LCURLY => self.make_token(TokenType::LeftBrace),
                RCURLY => self.make_token(TokenType::RightBrace),
                ADD => {
                    if self.peek(1).unwrap_or('\0') == EQUALS {
                        self.position += 1;
                        self.make_token(TokenType::PlusEqual)
                    }
                    else {
                        self.make_token(TokenType::Plus)
                    }
                },
                SUB => {
                    if self.peek(1).unwrap_or('\0') == EQUALS {
                        self.position += 1;
                        self.make_token(TokenType::MinusEqual)
                    }
                    else if self.peek(1).unwrap_or('\0') == CROC_CLOSE {
                        self.position += 1;
                        self.make_token(TokenType::Arrow)
                    }
                    else {
                        self.make_token(TokenType::Minus)
                    }
                },
                DIV => {
                    if self.peek(1).unwrap_or('\0') == EQUALS {
                        self.position += 1;
                        self.make_token(TokenType::SlashEqual)
                    }
                    else {
                        self.make_token(TokenType::Slash)
                }
                },
                MUL => {
                    if self.peek(1).unwrap_or('\0') == EQUALS {
                        self.position += 1;
                        self.make_token(TokenType::StarEqual)
                    }
                    else {
                        self.make_token(TokenType::Star)
                    }
                },
                MOD => {
                    if self.peek(1).unwrap_or('\0') == EQUALS {
                        self.position += 1;
                        self.make_token(TokenType::PercentEqual)
                    }
                    else {
                        self.make_token(TokenType::Percent)
                    }
                },
                AND => {
                    if self.peek(1).unwrap_or('\0') == EQUALS {
                        self.position += 1;
                        self.make_token(TokenType::AmpersandEqual)
                    }
                    else if self.peek(1).unwrap_or('\0') == AND {
                        self.position += 1;
                        self.make_token(TokenType::And)
                    }
                    else {
                        self.make_token(TokenType::Ampersand)
                    }
                },
                OR => {
                    if self.peek(1).unwrap_or('\0') == EQUALS {
                        self.position += 1;
                        self.make_token(TokenType::PipeEqual)
                    }
                    else if self.peek(1).unwrap_or('\0') == OR {
                        self.position += 1;
                        self.make_token(TokenType::Or)
                    }
                    else {
                        self.make_token(TokenType::Pipe)
                    }
                },
                XOR => {
                    if self.peek(1).unwrap_or('\0') == EQUALS {
                        self.position += 1;
                        self.make_token(TokenType::CaretEqual)
                    }
                    else {
                        self.make_token(TokenType::Caret)
                    }
                },
                NOT => {
                    if self.peek(0).unwrap_or('\0') == EQUALS {
                        self.position += 1;
                        self.make_token(TokenType::BangEqual)
                    }
                    else {
                        self.make_token(TokenType::Bang)
                    }
                },
                DOT => {
                    if self.peek(1).unwrap_or('\0') == DOT && self.peek(2).unwrap_or('\0') == DOT {
                        self.position += 2;
                        self.make_token(TokenType::ThreeDots)
                    }
                    else {
                        self.make_token(TokenType::Dot)
                    }
                },
                COLON => {
                    if self.peek(1) == Some(COLON) {
                        self.position += 2;
                        self.make_token(TokenType::DoubleColon)
                    }
                    else {
                        self.make_token(TokenType::Colon)
                    }
                },
                EQUALS => {
                    if self.peek(1).unwrap_or('\0') == EQUALS {
                        self.position += 1;
                        self.make_token(TokenType::EqualEqual)
                    }
                    else {
                        self.make_token(TokenType::Equal)
                    }
                },
                BANG => {
                    if self.peek(1).unwrap_or('\0') == EQUALS {
                        self.position += 1;
                        self.make_token(TokenType::BangEqual)
                    }
                    else {
                        self.make_token(TokenType::Bang)
                    }
                },
                LBRAC => self.make_token(TokenType::LeftParen),
                RBRAC => self.make_token(TokenType::RightParen),
                CROC_OPEN => {
                    if self.peek(1).unwrap_or('\0') == EQUALS {
                        self.position += 1;
                        self.make_token(TokenType::LessEqual)
                    }
                    else {
                        self.make_token(TokenType::Less)
                    }
                },
                CROC_CLOSE => {
                    if self.peek(1).unwrap_or('\0') == EQUALS {
                        self.position += 1;
                        self.make_token(TokenType::GreaterEqual)
                    }
                    else {
                        self.make_token(TokenType::Greater)
                    }
                },
                SEMI_COLON => self.make_token(TokenType::Semicolon),
                '"' => {
                    self.position += 1;
                    let lexed_string = self.lex_string();
                    self.position += 1;
                    lexed_string
                },
                // lex identifier
                'a'..='z' | 'A'..='Z' | '_' => {
                    // TODO: at some point, in the current "test.dl", the function "take" within the trait
                    //       contains a "(". This should not happen. Fix this.
                    while self.peek(1).unwrap_or('\0').is_alphanumeric() || self.peek(1).unwrap_or('\0') == '_' {
                        self.position += 1;
                    }
                    let identifier = &self.source[self.start_position..self.position + 1]
                        .trim()
                        .chars()
                        .into_iter()
                        .filter(|c| {
                            match c {
                                'a'..='z' | 'A'..='Z' | '_' | '0'..='9' => true,
                                _ => false
                            }
                        })
                        .collect::<String>();
                    let token_type = KEYWORDS
                        .iter()
                        .find(|(k, _)| *k == *identifier)
                        .map(|(_, v)| v.clone())
                        .unwrap_or(TokenType::Identifier((*identifier).to_owned()));
                    self.make_token(token_type)
                },
                ' ' | '\r' | '\t' => {
                    self.start_position += 1;
                    self.position += 1;
                    continue;
                },
                '\n' => {
                    self.location.line += 1;
                    self.location.column = 0;
                    self.start_position += 1;
                    self.position += 1;
                    continue;
                },
                '0'..='9' => {
                    while self.peek(1).unwrap_or('\0').is_numeric() {
                        self.position += 1;
                    }
                    let number_str = &self.source[self.start_position..self.position + 1]
                        .trim()
                        .chars()
                        .into_iter()
                        .filter(|ch| {
                            match ch {
                                '0'..='9' => true,
                                _ => false,
                            }
                        })
                        .collect::<String>();
                    let number = match number_str.parse::<usize>() {
                        Ok(n) => n,
                        Err(e) => panic!("failed to parse number: {} (value: {})", e, number_str)
                    };
                    self.make_token(TokenType::Number(number))
                },
                COMMA => self.make_token(TokenType::Comma),
                _ => panic!("unexpected character: {}", c)
            };

            self.tokens.push(token);
        }

        self.tokens.push(Token {
            token_type: TokenType::Eof,
            span: Span {
                start: self.position,
                end: self.position,
            },
            location: self.location.clone(),
        });
    } 

    // helper methods
    fn make_token(&mut self, token_type: TokenType) -> Token {
        let span = Span {
            start: self.start_position,
            end: self.position + 1,
        };
        let token = Token {
            token_type,
            span,
            location: self.location.clone(),
        };
        self.start_position = self.position;
        self.position += 1;
        token
    }

    fn lex_string(&mut self) -> Token {
        let mut current_pos = self.position;
        while let Some(ch) = self.at(current_pos) {
            if ch == '"' {
                break;
            }
            current_pos += 1;
        }
        let string = &self.source[self.start_position..current_pos + 1].trim();
        self.position = current_pos - 1 ;
        self.make_token(TokenType::String((*string).to_owned()))
    }

    fn at(&self, position: usize) -> Option<char> {
        self.source.chars().nth(position)
    }

    fn peek(&self, ahead: usize) -> Option<char> {
        let result = self.source.chars().nth(self.position + ahead);
        match result {
            Some(c) => Some(c),
            None => panic!("peek() called at EOF."),
        }
    }

    fn current(&self) -> Option<char> {
        self.source.chars().nth(self.position)
    }

    fn fail(&self, message: &str) {
        println!("LEXER: {}:{}:{}: {}", self.location.file, self.location.line, self.location.column, message);
    }
}