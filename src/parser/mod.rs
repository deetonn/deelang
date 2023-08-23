pub mod ast_node;
pub mod expressions;

use std::cell::Cell;

use ast_node::*;
use crate::exec::ExtraInfo;
use crate::tokenizer::{Token, SourceLocation, token_type::TokenType};
use crate::err::{display_fatal, display_warning};

use expressions::*;

#[derive(Clone, Debug, PartialEq)]
pub enum ParserError {
    // expected `something`
    ExpectedXXX(String),
    // Signifies that the parser has finished parsing the file
    Done,
    NotImplemented,
}

type ParseResult<T> = Result<T, ParserError>;

impl ParserError {
    pub fn to_string(&self) -> String {
        match self {
            ParserError::ExpectedXXX(message) => format!("expected {}", message),
            ParserError::Done => "done".to_owned(),
            ParserError::NotImplemented => "not implemented".to_owned(),
        }
    }
}

trait IntoSignatureTypes {
    fn into_signature_types(&self) -> Vec<TypeInfo>;
}

impl IntoSignatureTypes for Vec<ParameterFacts> {
    fn into_signature_types(&self) -> Vec<TypeInfo> {
        let mut types: Vec<TypeInfo> = Vec::new();
        for param in self {
            types.push(param.type_info.clone());
        }
        types
    }
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

    fn walk_back(&self, amount: usize) {
        self.position.set(self.position.get() - amount);
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

pub fn checked_integral_cast(from: usize, info: &TypeInfo) -> ParseResult<Box<dyn Expression>> {
    match info.name.as_str() {
        "bool" => {
            if from > 1 {
                return Err(ParserError::ExpectedXXX(format!("bool, found `{}`. NOTE: implicit casts to bool must evaluate to `0` or `1`.", from)));
            }
            return Ok(
                BoolLiteralExpression::into_expr(from == 1)
            )
        },
        "char" => {
            if from > u8::MAX as usize {
                return Err(ParserError::ExpectedXXX(format!("char, found `{}`. NOTE: anything between 0-{} is valid here.", u8::MAX, from)));
            }
            return Ok(
                CharLiteralExpression::into_expr(from as u8 as char)
            )
        },
        "i32" => {
            if from > i32::MAX as usize {
                return Err(ParserError::ExpectedXXX(format!("i32, found `{}`. NOTE: This value is larger than the maximum of u32 ({})", from, i32::MAX)));
            }
            return Ok(
                I32LiteralExpression::into_expr(from as i32)
            )
        },
        "u32" => {
            if from > u32::MAX as usize {
                return Err(
                    ParserError::ExpectedXXX(
                        format!("u32, found `{}`. NOTE: This value is larger than the maximum for this type. (The max: {}) try using u64.", from, u32::MAX)
                    )
                );
            }
            return Ok(
                U32LiteralExpression::into_expr(from as u32)
            )
        },
        "i64" => {
            if from > i64::MAX as usize {
                return Err(ParserError::ExpectedXXX(format!("i64, found `{}`", from)));
            }
            return Ok(
                I64LiteralExpression::into_expr(from as i64)
            )
        },
        "u64" => {
            if from > u64::MAX as usize {
                return Err(ParserError::ExpectedXXX(format!("u64, found `{}`", from)));
            }
            return Ok(
                U64LiteralExpression::into_expr(from as u64)
            )
        },
        _ => {
            return Err(ParserError::ExpectedXXX(format!("integral type, found `{}`", info.name)));
        }
    }
}

pub fn parse_string_literal(_: &ParseContext,
    string: &String,
    expr_context: &ExpressionContext
) -> ParseResult<AstNode> {
    if let Some(requested_type) = &expr_context.typ {
        if requested_type.is_builtin_integral() {
            return Err(ParserError::ExpectedXXX(format!("string, cannot implicity convert a string literal into an integral type.")));
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
) -> ParseResult<AstNode> {
    if let Some(requested_type) = &expr_context.typ {
        // The user wants this to be a specific type, so we should try to parse it as that type.
        if !requested_type.is_builtin_integral() {
            return Err(ParserError::ExpectedXXX(format!("integral type, found `{}`", requested_type.name)));
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

// Due to semi-colons being optional in certain scenarios, this function is expected to consume the semi-colon.
// If a semi-colon is not found, that is fine. We assume that the semi-colon is optional.
pub fn parse_semicolon(context: &ParseContext) {
    let next = context.advance();
    if let Some(next) = next {
        if matches(next, TokenType::Semicolon) {
            return;
        }
    }
    context.position.set(context.position.get() - 1);
}

// This function is expected to parse an expression and consume the semi-colon.
pub fn parse_expression(context: &ParseContext, expr_context: &ExpressionContext) -> ParseResult<AstNode> {
    let next_token = match context.advance() {
        Some(token) => token,
        None => return Err(ParserError::ExpectedXXX("expression".to_owned())),
    };

    match &next_token.token_type {
        TokenType::Number(num) => {
            let number_expr = parse_number_expression(context, *num, expr_context)?;
            let _ = parse_semicolon(context);
            Ok(number_expr)
        },
        TokenType::String(contents) => {
            let string_literal_expr = parse_string_literal(context, contents, expr_context);
            let _ = parse_semicolon(context);
            string_literal_expr
        },
        _ => todo!("unimplemented expression type")
    }
}

pub fn parse_assignment(context: &ParseContext, constant: bool) -> ParseResult<AstNode> {
    let identifier = match context.advance() {
        Some(Token { token_type: TokenType::Identifier(ident), .. }) => ident,
        _ => return Err(ParserError::ExpectedXXX(format!("identifier after `{}`", if constant { "const" } else { "let" }))),
    }; 

    let mut annotation: Option<TypeInfo> = None;

    if let Some(colon) = context.peek() {
        if matches(colon, TokenType::Colon) {
            context.advance();
            annotation = parse_typename(context);

            if annotation.is_none() {
                return Err(ParserError::ExpectedXXX(format!("type after `:`")));
            }
        }
    }

    // equals
    let _ = match context.advance() {
        Some(Token { token_type: TokenType::Equal, .. }) => true,
        _ => return Err(ParserError::ExpectedXXX(format!("equals sign for assignment of `{}`", identifier))),
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

    // parse_expression is expected to consume the semi-colon.
    // let semi = ...;

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

pub fn parse_function_declaration_parameter(context: &ParseContext) -> ParseResult<ParameterFacts> {
    let identifier = match context.advance() {
        Some(Token { token_type: TokenType::Identifier(ident), .. }) => ident,
        _ => return Err(ParserError::ExpectedXXX("identifier".to_owned())),
    };

    let _ = match context.advance() {
        Some(Token { token_type: TokenType::Colon, .. }) => true,
        _ => return Err(ParserError::ExpectedXXX("colon, parameters within a declaration require a type annotation.".to_owned())),
    };

    let typ = parse_typename(context);

    if typ.is_none() {
        return Err(ParserError::ExpectedXXX("type after `:`. NOTE: type annotations are required for declaration parameters".to_owned()));
    }

    Ok(
        ParameterFacts {
            name: identifier.clone(),
            type_info: match typ {
                Some(typ) => typ,
                None => return Err(ParserError::ExpectedXXX("type after `:`. NOTE: type annotations are required for declaration parameters".to_owned())),
            },
            initializer: None
        }
    )
}

pub fn parse_body(context: &ParseContext, owner: Option<Signature>) -> ParseResult<AstNode> {
    let _ = match context.advance() {
        Some(Token { token_type: TokenType::LeftBrace, .. }) => true,
        _ => return Err(ParserError::ExpectedXXX("left brace".to_owned())),
    };

    let mut nodes: Vec<AstNode> = Vec::new();

    loop {
        if let Some(curr_tok) = context.peek() {
            if matches(curr_tok, TokenType::RightBrace) {
                _ = context.advance();
                break;
            }
        }

        let stmt = parse_statement(&context);
        if let Ok(stmt) = stmt {
            nodes.push(stmt);
        }
        else if stmt.is_err() && stmt.clone().unwrap_err() == ParserError::Done {
            break;
        }
        else {
            let mut source_loc = context.get_source_location();
            source_loc.file = "test.dl".to_owned();
            display_fatal(&source_loc, 
                &format!("failed to parse statement: {} (at position {})", stmt.unwrap_err().to_string(), context.position.get()));
        }
    }

    Ok(
        AstNode::Body(
            Block {
                statements: nodes,
                owner
            }
        )
    )
}

pub fn parse_function_declaration(context: &ParseContext) -> ParseResult<AstNode> {
    // `fn` keyword.
    let _ = match context.advance() {
        Some(Token { token_type: TokenType::Fn, .. }) => true,
        _ => return Err(ParserError::ExpectedXXX("fn".to_owned())),
    };

    let identifier = match context.advance() {
        Some(Token { token_type: TokenType::Identifier(ident), .. }) => ident,
        _ => return Err(ParserError::ExpectedXXX("identifier".to_owned())),
    };

    let mut arguments: Vec<ParameterFacts> = Vec::new();
    let mut had_first_paren = false;

    loop {
        let current = context.advance();

        if let Some(Token { token_type: TokenType::LeftParen, .. }) = current {
            had_first_paren = true;
            continue;
        }
        else if let Some(Token { token_type: TokenType::Comma, .. }) = current {
            continue;
        }
        else if let Some(Token { token_type: TokenType::RightParen, .. }) = current {
            if !had_first_paren {
                return Err(ParserError::ExpectedXXX("left parenthesis".to_owned()));
            }
            break;
        }
        else if let Some(Token { token_type: TokenType::Identifier(_), .. }) = current {
            context.position.set(context.position.get() - 1);
            let param = parse_function_declaration_parameter(context)?;
            arguments.push(param);
        }
        else {
            return Err(ParserError::ExpectedXXX("identifier, comma, or right parenthesis".to_owned()));
        }
    }

    let _ = match context.advance() {
        Some(Token { token_type: TokenType::Arrow, .. }) => true,
        _ => return Err(ParserError::ExpectedXXX("arrow(->), functions require a return type annotation.".to_owned())),
    };

    let return_type = match parse_typename(context) {
        Some(typ) => typ,
        None => return Err(ParserError::ExpectedXXX("type after `->`. NOTE: type annotations are required for function return types".to_owned())),
    };

    let body = parse_body(context, Some(
        Signature {
            name: identifier.clone(),
            arguments: arguments.into_signature_types(),
            return_type: return_type.clone(),
        }
    ))?;

    Ok(
        AstNode::FunctionDeclaration(
            FunctionDeclarationFacts {
                name: identifier.clone(),
                arguments,
                return_type: return_type.clone(),
                body: match body {
                    AstNode::Body(body) => body,
                    _ => panic!("invalid body")
                },
            }
        )
    )
}

pub fn parse_use_statement(context: &ParseContext) -> ParseResult<AstNode> {
    let _ = match context.advance() {
        Some(Token { token_type: TokenType::Use, .. }) => true,
        _ => return Err(ParserError::ExpectedXXX("use".to_owned())),
    };

    let identifier = match context.advance() {
        Some(Token { token_type: TokenType::Identifier(ident), .. }) => ident,
        _ => return Err(ParserError::ExpectedXXX("identifier".to_owned())),
    };

    let next = match context.advance() {
        Some(next) => next,
        None => return Err(ParserError::ExpectedXXX("identifier".to_owned())),
    };

    if matches(next, TokenType::Equal) {
        // This is a type alias
        let typename = match parse_typename(context) {
            Some(typename) => typename,
            None => return Err(ParserError::ExpectedXXX("type after `=`".to_owned())),
        };

        Ok(
            AstNode::TypeAlias(
                TypeAliasFacts {
                    name: identifier.clone(),
                    type_info: typename.clone(),
                }
            )
        )
    }
    else if matches(next, TokenType::DoubleColon) {
        // This is a use module::statement::...
        let mut abstract_path: Vec<String> = Vec::new();
        abstract_path.push(identifier.clone());

        loop {
            let next = match context.advance() {
                Some(next) => next,
                None => return Err(ParserError::ExpectedXXX("identifier".to_owned())),
            };

            match &next.token_type {
                TokenType::Identifier(ident) => {
                    abstract_path.push(ident.clone());
                },
                TokenType::DoubleColon => {
                    continue;
                },
                _ => {
                    display_warning(
                        &next.location, 
                        &format!("possibly malformed use statement, expected `::` or identifier, found `{:?}`", next.token_type));
                    context.walk_back(1);
                    break;
                }
            }
        }

        Ok(
            AstNode::UseStatement(
                UseStatementFacts {
                    path: abstract_path,
                }
            )
        )
    }
    else {
        Err(ParserError::ExpectedXXX("malformed use statement.".to_owned()))
    }
}

pub fn parse_statement(context: &ParseContext) -> Result<AstNode, ParserError> {
    let next = match context.advance() {
        Some(next) => next,
        None => return Err(ParserError::ExpectedXXX("statement".to_owned())),
    };

    if matches(next, TokenType::Let) {
        return parse_assignment(context, false);
    }

    if matches(next, TokenType::Const) {
        return parse_assignment(context, true);
    }

    if matches(next, TokenType::Eof) {
        return Err(ParserError::Done);
    }

    if matches(next, TokenType::Fn) {
        // go back to the beginning of the function declaration
        context.walk_back(1);
        return parse_function_declaration(context);
    }

    if matches(next, TokenType::Use) {
        context.walk_back(1);
        return parse_use_statement(context);
    }

    if matches(next, TokenType::If) {
        todo!("Implement all of the conditional operators before implementing if statements.");
    }

    Err(ParserError::ExpectedXXX(format!("statement, but got token {:?}", next.token_type)))
}

pub fn parse(toks: Vec<Token>, extras: ExtraInfo) -> Vec<AstNode> {
    let context = ParseContext {
        tokens: toks,
        position: Cell::new(0),
    };

    let mut nodes: Vec<AstNode> = Vec::new();

    loop {
        let stmt = parse_statement(&context);
        if let Ok(stmt) = stmt {
            nodes.push(stmt);
        }
        else if stmt.is_err() && stmt.clone().unwrap_err() == ParserError::Done {
            break;
        }
        else {
            let mut source_loc = context.get_source_location();
            source_loc.file = extras.file_name;
            display_fatal(&source_loc, 
                &format!("failed to parse statement: {} (at position {})", stmt.unwrap_err().to_string(), context.position.get()));
        }
    }

    nodes
}