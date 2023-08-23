pub mod ast_node;
pub mod expressions;

use std::cell::Cell;

use ast_node::*;
use crate::tokenizer::{Token, SourceLocation, token_type::TokenType};
use crate::err::display_fatal;

use expressions::*;

#[derive(Clone, Debug, PartialEq)]
pub enum ParserError {
    // expected `something`
    ExpectedXXX(String),
    // Signifies that the parser has finished parsing the file
    Done,
    NotImplemented,
}

trait FilterForExpression {
    fn filter_for_expression(&self, current_pos: usize, terminator: Option<TokenType>) -> Option<Vec<Token>>;
}

impl FilterForExpression for &Vec<Token> {
    fn filter_for_expression(&self, current_pos: usize, terminator: Option<TokenType>) -> Option<Vec<Token>> {
        let position_of_semi = self.iter().position(|token| {
            token.token_type == if let Some(terminator) = &terminator {
                (*terminator).clone()
            }
            else {
                TokenType::Semicolon
            }
        });

        if let Some(pos) = position_of_semi {
            // clone all tokens before the terminator and insert it into a new vector
            let mut filtered_tokens: Vec<Token>= Vec::new();
            for i in current_pos..pos {
                filtered_tokens.push(
                    Token::from_other(
                        self[i].token_type.clone(), 
                        &self[i]
                    )
                );
            }
            return Some(filtered_tokens);
        }
        else {
            None
        }
    }
}

pub struct ParseContext {
    pub tokens: Vec<Token>,
    pub position: Cell<usize>,
}

pub struct ExpressionContext<'a> {
    pub typ: &'a Option<TypeInfo>,
}

impl ParseContext {
    fn advance(&self) -> Option<&Token> {
        let pos = self.position.get();
        if pos >= self.tokens.len() {
            return None;
        }
        self.position.set(pos + 1);
        Some(&self.tokens[pos])
    }

    fn peek(&self) -> Option<&Token> {
        let pos = self.position.get();
        if pos >= self.tokens.len() {
            return None;
        }
        Some(&self.tokens[pos])
    }

    fn get_source_location(&self) -> SourceLocation {
        let pos = self.position.get();
        if pos >= self.tokens.len() {
            return SourceLocation {
                file: String::new(),
                line: 0,
                column: 0,
            };
        }
        self.tokens[pos].location.clone()
    }
}

pub fn matches(token: &Token, typ: TokenType) -> bool {
    token.token_type == typ
}

pub fn parse_typename(context: &ParseContext) -> Option<TypeInfo> {
    let next = context.advance()?;

    match &next.token_type {
        TokenType::Identifier(ident) => {
            Some(TypeInfo {
                name: ident.clone(),
                size: 0,
                needs_to_resolve_size: true,
                has_been_resolved: false,
            })
        },
        _ => None,
    }
}

pub fn checked_integral_cast(from: usize, info: &TypeInfo) -> Result<Box<dyn Expression>, ParserError> {
    match info.name.as_str() {
        "bool" => {
            if from > 1 {
                return Err(ParserError::ExpectedXXX(format!("expected bool, found `{}`. NOTE: implicit casts to bool must evaluate to `0` or `1`.", from)));
            }
            return Ok(
                BoolLiteralExpression::into_expr(from == 1)
            )
        },
        "char" => {
            if from > u8::MAX as usize {
                return Err(ParserError::ExpectedXXX(format!("expected char, found `{}`. NOTE: anything between 0-{} is valid here.", u8::MAX, from)));
            }
            return Ok(
                CharLiteralExpression::into_expr(from as u8 as char)
            )
        },
        "i32" => {
            if from > i32::MAX as usize {
                return Err(ParserError::ExpectedXXX(format!("expected i32, found `{}`. NOTE: This value is larger than the maximum of u32 ({})", from, i32::MAX)));
            }
            return Ok(
                I32LiteralExpression::into_expr(from as i32)
            )
        },
        "u32" => {
            if from > u32::MAX as usize {
                return Err(
                    ParserError::ExpectedXXX(
                        format!("expected u32, found `{}`. NOTE: This value is larger than the maximum for this type. (The max: {}) try using u64.", from, u32::MAX)
                    )
                );
            }
            return Ok(
                U32LiteralExpression::into_expr(from as u32)
            )
        },
        "i64" => {
            if from > i64::MAX as usize {
                return Err(ParserError::ExpectedXXX(format!("expected i64, found `{}`", from)));
            }
            return Ok(
                I64LiteralExpression::into_expr(from as i64)
            )
        },
        "u64" => {
            if from > u64::MAX as usize {
                return Err(ParserError::ExpectedXXX(format!("expected u64, found `{}`", from)));
            }
            return Ok(
                U64LiteralExpression::into_expr(from as u64)
            )
        },
        _ => {
            return Err(ParserError::ExpectedXXX(format!("expected integral type, found `{}`", info.name)));
        }
    }
}

pub fn parse_string_literal(_: &ParseContext,
    string: &String,
    expr_context: &ExpressionContext
) -> Result<AstNode, ParserError> {
    if let Some(requested_type) = &expr_context.typ {
        if requested_type.is_builtin_integral() {
            return Err(ParserError::ExpectedXXX(format!("cannot implicity convert a string literal into an integral type.")));
        }
    }

    return Ok(
        AstNode::Expression(
            StringLiteralExpression::into_expr(string.clone())
        )
    )
}

pub fn parse_number_expression(_: &ParseContext,
     number: usize, 
     expr_context: &ExpressionContext
) -> Result<AstNode, ParserError> {
    if let Some(requested_type) = &expr_context.typ {
        // The user wants this to be a specific type, so we should try to parse it as that type.
        if !requested_type.is_builtin_integral() {
            return Err(ParserError::ExpectedXXX(format!("expected integral type, found `{}`", requested_type.name)));
        }
        return match checked_integral_cast(number, requested_type) {
            Ok(expr) => {
                Ok(
                    AstNode::Expression(
                        expr
                    )
                )
            },
            Err(err) => Err(err),
        };
    }
    else {
        // We try to infer the type from the expression itself
        if number > u32::MAX as usize {
            // The number is too big to fit in a u32, so we should use an i64
            return Ok(
                AstNode::Expression(
                    U64LiteralExpression::into_expr(number as u64)
                )
            )
        }
        else {
            // The number is small enough to fit in a u32, so we should use an i32
            return Ok(
                AstNode::Expression(
                    U32LiteralExpression::into_expr(number as u32)
                )
            )
        }
    }
}

pub fn parse_expression(context: &ParseContext, expr_context: &ExpressionContext) -> Result<AstNode, ParserError> {
    let next_token = match context.advance() {
        Some(token) => token,
        None => return Err(ParserError::ExpectedXXX("expected expression".to_owned())),
    };

    match &next_token.token_type {
        TokenType::Number(num) => parse_number_expression(context, num.clone(), expr_context),
        TokenType::String(contents) => parse_string_literal(context, contents, expr_context),
        _ => todo!("unimplemented expression type")
    }
}

pub fn parse_assignment(context: &ParseContext, constant: bool) -> Result<AstNode, ParserError> {
    let identifier = match context.advance() {
        Some(Token { token_type: TokenType::Identifier(ident), .. }) => ident,
        _ => return Err(ParserError::ExpectedXXX(format!("expected identifier after `{}`", if constant { "const" } else { "let" }))),
    }; 

    let mut annotation: Option<TypeInfo> = None;

    if let Some(colon) = context.peek() {
        if matches(colon, TokenType::Colon) {
            context.advance();
            annotation = parse_typename(context);
        }
    }

    // equals
    let _ = match context.advance() {
        Some(Token { token_type: TokenType::Equal, .. }) => true,
        _ => return Err(ParserError::ExpectedXXX(format!("Expected `=` after identifier `{}`", identifier))),
    };

    let expr_context = ExpressionContext {
        typ: &annotation,
    };

    let expression = parse_expression(context, &expr_context)?;

    let expression_storage = match expression {
        AstNode::Expression(expr) => {
            if annotation.is_none() {
                if let Some(typ) = &expr.try_evaluate_type() {
                    annotation = Some(typ.clone());
                }
            }
            Some(expr)
        },
        _ => None
    };

    return Ok(
        AstNode::Assignment(
            AssignmentFacts {
                name: identifier.clone(),
                type_info: annotation,
                constant,
                expression: match expression_storage {
                    Some(expr) => expr,
                    None => panic!("invalid expression")
                },
            }
        )
    )
}

pub fn parse_statement(context: &ParseContext) -> Result<AstNode, ParserError> {
    let next = match context.advance() {
        Some(next) => next,
        None => return Err(ParserError::ExpectedXXX("statement".to_owned())),
    };
    println!("next: {:?}", next);

    if matches(next, TokenType::Let) {
        return parse_assignment(context, false);
    }

    if matches(next, TokenType::Const) {
        return parse_assignment(context, true);
    }

    Err(ParserError::ExpectedXXX("expected statement".to_owned()))
}

pub fn parse(toks: Vec<Token>) -> Vec<AstNode> {
    let context = ParseContext {
        tokens: toks,
        position: Cell::new(0),
    };

    let mut nodes: Vec<AstNode> = Vec::new();

    let stmt = parse_statement(&context);
    if let Ok(stmt) = stmt {
        nodes.push(stmt);
    }
    else {
        display_fatal(&context.get_source_location(), 
            &format!("failed to parse statement: {:?}", stmt));
    }

    nodes
}