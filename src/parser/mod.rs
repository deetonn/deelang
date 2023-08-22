pub mod ast_node;
pub mod expressions;

use ast_node::*;
use crate::tokenizer::{Token, SourceLocation, token_type::TokenType};
use crate::err::display_fatal;

use expressions::*;

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

pub struct Parser {
    pub ast: Vec<AstNode>,
    pub tokens: Vec<Token>,
    pub current: usize,
}

macro_rules! take_or_fail {
    // This macro just replaces the following code:
    // let token = match self.next() {
    //     Some(token) => token.clone(),
    //     None => display_fatal(SourceLocation::default(), "Expected identifier after let.")
    // };
    // with this:
    // let token = pofb!(self.next(), "Expected identifier after let.");
    ($self: ident, $expr:expr, $message:expr) => {
        match $expr {
            Some(token) => {
                token.clone()
            },
            None => display_fatal(&SourceLocation::default(), $message),
        }
    };
}

pub struct ExpressionInput<'a> {
    pub tokens: &'a Vec<Token>,
    pub position: &'a mut usize,
    pub type_info: Option<TypeInfo>,
}

impl<'a> ExpressionInput<'a> {
    pub fn new(tokens: &'a Vec<Token>, position: &'a mut usize) -> ExpressionInput<'a> {
        ExpressionInput {
            tokens,
            position,
            type_info: None,
        }
    }

    pub fn with_type_info(tokens: &'a Vec<Token>, position: &'a mut usize, type_info: Option<TypeInfo>) -> ExpressionInput<'a> {
        ExpressionInput {
            tokens,
            position,
            type_info,
        }
    }
}

pub fn parse_inferred_number_expression(num: usize) -> Box<dyn Expression> {
    // if the number is not negative, its automatically unsigned.
    // if this unsigned number is larger than u32::MAX, it will be interpreted as u64.
    // otherwise, it will be interpreted as u32. Currently signed numbers cannot work.
    if num <= std::u32::MAX as usize {
        return Box::new(U32LiteralExpression {
            value: num as u32,
        });
    }
    else {
        return Box::new(U64LiteralExpression {
            value: num as u64,
        });
    }
}

pub fn parse_expression(input: ExpressionInput, terminator: Option<TokenType>) -> Option<Box<dyn Expression>> {
    // collect all the tokens until we reach a semicolon
    // then parse the expression
    let expression_tokens = input.tokens.filter_for_expression(*input.position, terminator)?;

    let is_simple = expression_tokens.len() == 1;

    if is_simple {
        let simple_token = &expression_tokens[0];

        if let TokenType::Identifier(spelling) = simple_token.token_type.clone() {
            return Some(Box::new(VariableReferenceExpression {
                name: spelling.clone(),
                assigne_type: input.type_info.clone(),
            }));
        }

        if let TokenType::Number(num) = simple_token.token_type.clone() {
            if let Some(type_info) = input.type_info {
                if type_info.name == "u32" {
                    return Some(Box::new(U32LiteralExpression {
                        value: num as u32,
                    }));
                }
                if type_info.name == "i32" {
                    return Some(Box::new(I32LiteralExpression {
                        value: num as i32,
                    }));
                }
            }
            else {
                return Some(parse_inferred_number_expression(num));
            }
        }

        panic!("unhandled simple expression: {:?}", simple_token.token_type);
    }

    /* The expression is not simple, parse individual branches */

    /* If the expression begins with an identifier, it can either be:
         1. A function call
         2. A chaining `.` operator
    */

    // for function calls we expect <ident><open_paren><expressions...><close_paren>

    let first: &Token = &expression_tokens[0];
    if let TokenType::Identifier(first) = &first.token_type {
        let possible_paren = &expression_tokens[1];
        if let TokenType::LeftParen = possible_paren.token_type {
            let mut current_index = 2;
            let mut expressions = Vec::new();
            while let Some(token) = expression_tokens.get(current_index) {
                if let TokenType::RightParen = token.token_type {
                    break;
                }
                let expr = parse_expression(
                    ExpressionInput::new(&expression_tokens, &mut current_index), 
                    Some(TokenType::Comma));
                if let Some(expr) = expr {
                    expressions.push(expr);
                }
                else {
                    display_fatal(&token.location, "Expected expression after comma.");
                }
                current_index += 1;
            }
            return Some(Box::new(FunctionCallExpression {
                name: first.clone(),
                arguments: expressions,
            }));
        }
    }
    unimplemented!("unhandled expression: {:?}", expression_tokens);
}

pub fn parse_type_annotation(tokens: &Vec<Token>, position: &mut usize) -> Option<TypeInfo> {
    let current_token = &tokens[*position];
    let mut info = match current_token.token_type {
        TokenType::Identifier(_) => {
            *position += 1;
            Some(TypeInfo {
                name: match current_token.token_type.clone() {
                    TokenType::Identifier(name) => name.clone(),
                    _ => display_fatal(&current_token.location, "Expected identifier after colon."),
                },
                size: 0,
                /* The size will be resolved once types have been verified. */
                /* This needs to happen just before interpreting. */
                needs_to_resolve_size: true,
                has_been_resolved: true,
            })
        },
        _ => None,
    }?;

    info.try_resolve_size();
    Some(info)
}

pub struct ParseResult {
    pub node: AstNode,
    pub next_index: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            ast: Vec::new(),
            tokens,
            current: 0,
        }
    }

    pub fn parse(&mut self) {
        while self.current_token(self.current).is_some() {
            let statement = self.parse_statement();
            dbg!(&statement);
            self.ast.push(statement);
        }
    }

    // utility

    fn parse_statement(&mut self) -> AstNode {
        let current_index = self.current.clone();
        let mut prev_result: Option<ParseResult> = None;

        if self.matches(TokenType::Let, current_index) {
            self.current += 1;
            prev_result = Some(self.parse_assignment(false));
        }
        if self.matches(TokenType::Const, current_index) {
            self.current += 1;
            prev_result = Some(self.parse_assignment(true));
        }

        if let Some(result) = prev_result {
            self.current = result.next_index;
            return result.node;
        }

        todo!("add more statements: current token: {:?}", self.current_token(self.current));
    }

    // TODO: fix this shit, so many mutable this mutable that I DONT UNDERSTAND
    fn parse_assignment(&mut self, is_const: bool) -> ParseResult {
        let mut current_index = self.current.clone();

        let name = take_or_fail!(self, self.next(current_index), "Expected identifier after let.");
        current_index += 1;
        let mut annotation: Option<TypeInfo> = None;

        if self.matches(TokenType::Colon, current_index) {
            current_index += 1;
            annotation = parse_type_annotation(&self.tokens, &mut current_index);
        }

        let _: &Token = take_or_fail!(self, self.next(current_index), "Expected equals sign after identifier.");
        current_index += 1;
        let expr = parse_expression(
            ExpressionInput::with_type_info(&self.tokens, &mut current_index, annotation.clone()), 
            Some(TokenType::Semicolon));

        /* If no type annotation is present, try to evaluate the type via the expression */
        if let Some(expression) = &expr {
            if annotation.is_none() {
                annotation = expression.try_evaluate_type();
            }
        }

        if annotation.is_none() {
            dbg!(&expr);
            display_fatal(&name.location, "This declaration requires a type annotation. The type could not be inferred.");
        }

        let name_token = name.clone();
        return ParseResult { 
            node: AstNode::Assignment(AssignmentFacts {
                is_const,
                name: match name.token_type.clone() {
                    TokenType::Identifier(name) => name.clone(),
                    _ => display_fatal(&name_token.location, "Expected identifier after let."),
                },
                type_info: annotation,
                value: match expr {
                    Some(expr) => expr,
                    None => display_fatal(&name_token.location, "Expected expression after equals sign."),
                },
        }), next_index: current_index };
    }

    fn peek(&self, ahead: usize, pos: usize) -> Option<&Token> {
        let index = pos + ahead;
        if index >= self.tokens.len() {
            return None;
        }
        Some(&self.tokens[index])
    }

    fn current_token(&self, pos: usize) -> Option<&Token> {
        self.peek(0, pos)
    }

    fn next(&self, pos: usize) -> Option<&Token> {
        self.current_token(pos)
    }

    fn matches(&self, token_type: TokenType, pos: usize) -> bool {
        if self.peek(0, pos).unwrap_or(&Token::default()).token_type == token_type {
            return true;
        }
        false
    }

}