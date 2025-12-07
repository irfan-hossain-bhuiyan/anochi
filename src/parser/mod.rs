mod parser_error;

#[cfg(test)]
mod block_tests;

use crate::ast::{
    BinaryOperator, Expression, ExpressionNode, Statement, StatementBlock, StatementNode,
    UnaryOperator,
};
use crate::token::token_type::Keyword::{self, And, Or};
use crate::token::{Position, Token, TokenSlice, TokenType};
pub use parser_error::*;

/// Macro for token matching using matches! syntax
macro_rules! match_token {
    ($self:expr, $pattern:pat) => {{
        match $self.peek_type() {
            Some(current_token_type) => {
                if matches!(current_token_type, $pattern) {
                    $self.advance();
                    Ok(&$self.previous().token_type)
                } else {
                    Err(NoTokenFound::new(current_token_type.clone()))
                }
            }
            None => Err(NoTokenFound::NONE_TOKEN),
        }
    }};
}

macro_rules! match_token_or_err {
    ($self:expr, $pattern:pat) => {{
        let pos = $self.peek_position();
        match_token!($self, $pattern).map_err(|x| x.into_parser_error(pos))
    }};
}

pub struct Parser<'a> {
    tokens: &'a TokenSlice,
    current: usize,
    is_error: bool,
    is_in_loop: bool,
    //DESIGN DECISION:It is here because the vm,might go inside a function inside loop,if the
    //funciton has break,continue the vm will validate it,because it is inside loop.
}
type ExpNode = ExpressionNode;
type Exp = Expression<Position>;
type Stat = Statement<Position>;
type ReExp = Result<Exp, ParserError>;
type ReExpNode = Result<ExpNode, ParserError>;
type StatNode = StatementNode;
type ReStatNode = Result<StatementNode, ParserError>;
type StatBlock = StatementBlock<Position>;

pub enum ExprLevel {
    TypeUnion,
    LogicalOr,
    LogicalAnd,
    Equality,
    Comparison,
    Additive,
    Multiplicative,
    Unary,
    MemberAccess,
    Primary,
}
impl<'a> Parser<'a> {
    fn make_expr_node(&self, expr: Exp, start: usize) -> ExpNode {
        let slice = self.tokens.slice(start, self.current).pos_range();
        expr.to_node(slice)
    }

    fn parse_expr_level(&mut self, level: ExprLevel) -> ReExpNode {
        let start = self.current;
        match level {
            ExprLevel::TypeUnion => {
                let mut types = vec![self.parse_expr_level(ExprLevel::LogicalOr)?];
                while match_token!(self, TokenType::Pipe).is_ok() {
                    types.push(self.parse_expr_level(ExprLevel::LogicalOr)?);
                }
                if types.len() == 1 {
                    Ok(types.into_iter().next().unwrap())
                } else {
                    let expr = Expression::sum(types);
                    Ok(self.make_expr_node(expr, start))
                }
            }
            ExprLevel::LogicalOr => {
                let mut node = self.parse_expr_level(ExprLevel::LogicalAnd)?;
                while match_token!(self, TokenType::Keyword(Or)).is_ok() {
                    let right = self.parse_expr_level(ExprLevel::LogicalAnd)?;
                    let expr = Expression::binary(node, BinaryOperator::Or, right);
                    node = self.make_expr_node(expr, start);
                }
                Ok(node)
            }
            ExprLevel::LogicalAnd => {
                let mut node = self.parse_expr_level(ExprLevel::Equality)?;
                while match_token!(self, TokenType::Keyword(And)).is_ok() {
                    let right = self.parse_expr_level(ExprLevel::Equality)?;
                    let expr = Expression::binary(node, BinaryOperator::And, right);
                    node = self.make_expr_node(expr, start);
                }
                Ok(node)
            }
            ExprLevel::Equality => {
                let mut node = self.parse_expr_level(ExprLevel::Comparison)?;
                while let Ok(op) = match_token!(self, TokenType::EqualEqual | TokenType::BangEqual)
                {
                    let operator = match op {
                        TokenType::EqualEqual => BinaryOperator::Equal,
                        TokenType::BangEqual => BinaryOperator::NotEqual,
                        _ => unreachable!(),
                    };
                    let right = self.parse_expr_level(ExprLevel::Comparison)?;
                    let expr = Expression::binary(node, operator, right);
                    node = self.make_expr_node(expr, start);
                }
                Ok(node)
            }
            ExprLevel::Comparison => {
                let mut node = self.parse_expr_level(ExprLevel::Additive)?;
                while let Ok(op) = match_token!(
                    self,
                    TokenType::Less
                        | TokenType::LessEqual
                        | TokenType::Greater
                        | TokenType::GreaterEqual
                ) {
                    let operator = match op {
                        TokenType::Less => BinaryOperator::Less,
                        TokenType::LessEqual => BinaryOperator::LessEqual,
                        TokenType::Greater => BinaryOperator::Greater,
                        TokenType::GreaterEqual => BinaryOperator::GreaterEqual,
                        _ => unreachable!(),
                    };
                    let right = self.parse_expr_level(ExprLevel::Additive)?;
                    let expr = Expression::binary(node, operator, right);
                    node = self.make_expr_node(expr, start);
                }
                Ok(node)
            }
            ExprLevel::Additive => {
                let mut node = self.parse_expr_level(ExprLevel::Multiplicative)?;
                while let Ok(op) = match_token!(self, TokenType::Plus | TokenType::Minus) {
                    let operator = match op {
                        TokenType::Plus => BinaryOperator::Plus,
                        TokenType::Minus => BinaryOperator::Minus,
                        _ => unreachable!(),
                    };
                    let right = self.parse_expr_level(ExprLevel::Multiplicative)?;
                    let expr = Expression::binary(node, operator, right);
                    node = self.make_expr_node(expr, start);
                }
                Ok(node)
            }
            ExprLevel::Multiplicative => {
                let mut node = self.parse_expr_level(ExprLevel::Unary)?;
                while let Ok(op) = match_token!(self, TokenType::Star | TokenType::Slash) {
                    let operator = match op {
                        TokenType::Star => BinaryOperator::Multiply,
                        TokenType::Slash => BinaryOperator::Divide,
                        _ => unreachable!(),
                    };
                    let right = self.parse_expr_level(ExprLevel::Unary)?;
                    let expr = Expression::binary(node, operator, right);
                    node = self.make_expr_node(expr, start);
                }
                Ok(node)
            }
            ExprLevel::Unary => {
                let operator = match self.peek_type() {
                    Some(TokenType::Minus) => UnaryOperator::Minus,
                    Some(TokenType::Keyword(Keyword::Not)) => UnaryOperator::Not,
                    Some(TokenType::Ampersand) => UnaryOperator::Ref,
                    Some(TokenType::Star) => UnaryOperator::Deref,
                    _ => return self.parse_expr_level(ExprLevel::MemberAccess),
                };
                self.advance();
                let operand = self.parse_expr_level(ExprLevel::Unary)?;
                let expr = Expression::unary(operator, operand);
                Ok(self.make_expr_node(expr, start))
            }
            ExprLevel::MemberAccess => {
                let mut node = self.parse_expr_level(ExprLevel::Primary)?;
                if match_token!(self, TokenType::Bang).is_ok() {
                    let expr = self.parse_expression()?;
                    let fn_call = Expression::fn_call(node, expr);
                    return Ok(self.make_expr_node(fn_call, start));
                }
                while match_token!(self, TokenType::Dot).is_ok() {
                    let TokenType::Identifier(member) =
                        match_token_or_err!(self, TokenType::Identifier(_))?
                    else {
                        unreachable!()
                    };
                    let member = member.clone();
                    let expr = Expression::member_access(node, member);
                    node = self.make_expr_node(expr, start);
                }
                Ok(node)
            }
            ExprLevel::Primary => {
                let expr: Exp = match self
                    .peek_type()
                    .ok_or_else(|| self.error(ParserErrorType::NO_EXPN_FOUND))?
                {
                    TokenType::LeftBrace => self.parse_struct()?,
                    TokenType::LeftParen => {
                        self.advance();
                        let inner = self.parse_expression()?;
                        if let Some(TokenType::RightParen) = self.peek_type() {
                            self.advance();
                            Expression::grouping(inner)
                        } else {
                            return Err(self.error(ParserErrorType::expected_token_in_expression(
                                TokenType::RightParen,
                            )));
                        }
                    }
                    TokenType::Keyword(Keyword::Fn) => self.parse_function()?,
                    any => {
                        let expression = Expression::from_token_type(any.clone())
                            .ok_or_else(|| self.error(ParserErrorType::NO_EXPN_FOUND))?;
                        self.advance();
                        expression
                    }
                };
                Ok(self.make_expr_node(expr, start))
            }
        }
    }

    // Function ::= "|" Identifier "|" ("->" Expression)? "{" Statement "}"
    fn parse_function(&mut self) -> ReExp {
        assert!(
            matches!(self.peek_type(), Some(TokenType::Keyword(Keyword::Fn))),
            "parse_function called without Fn keyword"
        );
        self.advance();
        let input = self.parse_expr_level(ExprLevel::Primary)?;
        let output = if match_token!(self, TokenType::Arrow).is_err() {
            None
        } else {
            Some(Box::new(self.parse_expr_level(ExprLevel::Primary)?))
        };
        let statements = self.parse_statement()?;
        let input = Box::new(input);
        let statements = Box::new(statements);
        let expr = Expression::Function {
            input,
            output,
            statements,
        };
        Ok(expr)
    }

    fn make_stat_node(&self, stmt: Stat, start: usize) -> StatNode {
        let slice = self.tokens.slice(start, self.current).pos_range();
        stmt.to_node(slice)
    }

    fn parse_statement_block(&mut self) -> Result<StatBlock, ParserError> {
        match_token_or_err!(self, TokenType::LeftBrace)?;
        let mut statements = Vec::new();
        loop {
            if match_token!(self, TokenType::RightBrace).is_ok() {
                break;
            }
            match self.parse_statement() {
                Ok(x) => statements.push(x),
                Err(x) => return Err(x),
            }
        }
        Ok(StatementBlock { statements })
    }
    //fn into_parser_error(&self)
    pub fn parse_statement(&mut self) -> ReStatNode {
        let start = self.current;
        // Assignment: identifier = expression
        match self.peek_type().unwrap().clone() {
            TokenType::Keyword(Keyword::Let) => {
                self.advance();
                let TokenType::Identifier(x) = match_token_or_err!(self, TokenType::Identifier(_))?
                else {
                    unreachable!()
                };
                let mut r#type = None;
                let x = x.clone();
                if match_token! {self,TokenType::Colon}.is_ok() {
                    r#type = Some(self.parse_expression()?);
                }
                let _ = match_token_or_err!(self, TokenType::Equal)?;
                let expr = self.parse_expression()?;
                let _ = match_token_or_err!(self, TokenType::Semicolon)?;
                let stmt = Statement::assignment(x, r#type, expr);
                Ok(self.make_stat_node(stmt, start))
            }
            TokenType::Identifier(_) => {
                let x = self.parse_expression()?;
                // Check for typed assignment: identifier : type = value

                // Regular assignment: identifier = value
                let _ = match_token_or_err!(self, TokenType::Equal)?;
                let expr = self.parse_expression()?;
                let _ = match_token_or_err!(self, TokenType::Semicolon)?;
                let stmt = Statement::mutable_assignment(x, expr);
                Ok(self.make_stat_node(stmt, start))
            }
            TokenType::LeftBrace => match self.parse_statement_block() {
                Ok(x) => {
                    let stmt = Statement::StatementBlock(x);
                    Ok(self.make_stat_node(stmt, start))
                }
                Err(x) => Err(x),
            },
            TokenType::Keyword(Keyword::If) => {
                self.advance();
                let expr = self.parse_expression()?;
                let on_true = self.parse_statement()?;
                match match_token!(self, TokenType::Keyword(Keyword::Else)) {
                    Err(_) => {
                        let stmt = Statement::if_stmt(expr, on_true);
                        Ok(self.make_stat_node(stmt, start))
                    }
                    Ok(_) => {
                        let on_false = self.parse_statement()?;
                        let stmt = Statement::if_else(expr, on_true, on_false);
                        Ok(self.make_stat_node(stmt, start))
                    }
                }
            }
            TokenType::Keyword(Keyword::Debug) => {
                self.advance();
                let _ = match_token_or_err!(self, TokenType::LeftParen)?;
                let mut expr_vec = Vec::new();
                while let Ok(x) = self.parse_expr_level(ExprLevel::Additive) {
                    expr_vec.push(x);
                    if match_token!(self, TokenType::Comma).is_err() {
                        break;
                    }
                }
                let _ = match_token_or_err!(self, TokenType::RightParen)?;
                let _ = match_token_or_err!(self, TokenType::Semicolon)?;
                let stmt = Statement::debug(expr_vec);
                Ok(self.make_stat_node(stmt, start))
            }
            TokenType::Keyword(Keyword::Loop) => {
                self.advance();
                self.is_in_loop = true;
                let statement = self.parse_statement_block()?;
                self.is_in_loop = false;
                let stmt = Statement::Loop {
                    statements: statement,
                };
                Ok(self.make_stat_node(stmt, start))
            }
            TokenType::Keyword(Keyword::Break) => {
                if !self.is_in_loop {
                    return Err(
                        StatementParseErrorType::BreakOutsideLoop.with_pos(self.peek_position())
                    );
                }
                self.advance();
                match_token_or_err!(self, TokenType::Semicolon)?;
                let stmt = Statement::Break;
                Ok(self.make_stat_node(stmt, start))
            }
            TokenType::Keyword(Keyword::Continue) => {
                if !self.is_in_loop {
                    return Err(
                        StatementParseErrorType::ContinueOutsideLoop.with_pos(self.peek_position())
                    );
                }
                self.advance();
                let stmt = Statement::Continue;
                match_token_or_err!(self, TokenType::Semicolon)?;
                Ok(self.make_stat_node(stmt, start))
            }
TokenType::Keyword(Keyword::Return) => {
                self.advance();
                let expr = self.parse_expression().ok();
                match_token_or_err!(self, TokenType::Semicolon)?;
                let stmt = Statement::Return(expr);
                Ok(self.make_stat_node(stmt, start))
            }
            _ => Err(StatementParseErrorType::NoStatement.with_pos(self.peek_position())),
        }
    }

    pub fn new(tokens: &'a TokenSlice) -> Self {
        Parser {
            tokens,
            current: 0,
            is_error: false,
            is_in_loop: false,
        }
    }

    pub fn parse_expression(&mut self) -> ReExpNode {
        self.parse_expr_level(ExprLevel::TypeUnion)
    }

    /// Returns the next token (peek), or None if at end.
    fn peek_type(&self) -> Option<&TokenType> {
        self.peek().map(|x| &x.token_type)
    }

    // Struct ::= "{" (Identifier "=" Expression ("," Identifier "=" Expression)*)? "}"
    fn parse_struct(&mut self) -> ReExp {
        assert!(
            matches!(self.peek_type(), Some(TokenType::LeftBrace)),
            "parse_struct called without LeftBrace"
        );
        self.advance(); // consume '{'
        let mut fields = std::collections::HashMap::new();
        // Handle empty struct case
        if match_token!(self, TokenType::RightBrace).is_ok() {
            return Ok(Expression::product(fields));
        }

        loop {
            // Parse field name (identifier)
            let field_name = match match_token_or_err!(self, TokenType::Identifier(_))? {
                TokenType::Identifier(name) => name.clone(),
                _ => unreachable!(),
            };
            // Expect '='
            let _ = match_token_or_err!(self, TokenType::Equal)?;
            // Parse field value (expression)
            let field_value = self.parse_expression()?;
            // Add field to struct
            fields.insert(field_name, field_value);
            // Check for continuation or end
            match match_token!(self, TokenType::Comma | TokenType::RightBrace) {
                Ok(TokenType::Comma) => continue,   // More fields to parse
                Ok(TokenType::RightBrace) => break, // End of struct
                Ok(_) => unreachable!(),            // Should never happen due to pattern
                Err(_) => {
                    return Err(self.error(ParserErrorType::expected_token_in_expression(
                        TokenType::RightBrace,
                    )));
                }
            }
        }
        Ok(Expression::product(fields))
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn peek_position(&self) -> Position {
        self.peek().map(|t| t.position.clone()).unwrap_or_default()
    }

    fn error(&self, error_type: ParserErrorType) -> ParserError {
        error_type.with_pos(self.peek_position())
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    pub(crate) fn parse_statements(&mut self) -> ReStatNode {
        let start = self.current;
        let mut vec = Vec::new();
        while !self.is_at_end() {
            let stmt = self.parse_statement()?;
            vec.push(stmt);
        }
        let stmt = Statement::Statements(StatementBlock::new(vec));
        let stmt = self.make_stat_node(stmt, start);
        Ok(stmt)
    }
}
