mod parser_error;

//#[cfg(test)]
//mod block_tests;

use crate::ast::{
    BinaryOperator, Expression, ExpressionNode, Statement, StatementBlock, StatementNode,
    UnaryOperator,
};
use crate::token::token_type::Keyword::{self, And, Or};
use crate::token::{Token, TokenSlice, TokenType};
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

/// Macro for token matching with error conversion to ParserError
macro_rules! match_token_or_err {
    ($self:expr, $pattern:pat) => {{ match_token!($self, $pattern).map_err(|x| x.into_parser_error()) }};
}

pub struct Parser<'a> {
    tokens: &'a TokenSlice<'a>,
    current: usize,
    is_error: bool,
    is_in_loop: bool,
    //DESIGN DECISION:It is here because the vm,might go inside a function inside loop,if the
    //funciton has break,continue the vm will validate it,because it is inside loop.
}
type ExpNode<'a> = ExpressionNode<'a>;
type Exp<'a> = Expression<&'a TokenSlice<'a>>;
type Stat<'a> = Statement<&'a TokenSlice<'a>>;
type ReExpNode<'a> = Result<ExpNode<'a>, ParserError>;
type StatNode<'a> = StatementNode<'a>;
type ReStatNode<'a> = Result<StatementNode<'a>, ParserError>;
type StatBlock<'a> = StatementBlock<&'a TokenSlice<'a>>;
impl<'a> Parser<'a> {
    // Helper methods to reduce repetition
    fn make_expr_node(&self, expr: Exp<'a>, start: usize) -> ExpNode<'a> {
        let slice = self.tokens.slice(start, self.current);
        expr.to_node(slice)
    }

    // Function ::= "|" Identifier "|" ("->" Expression)? "{" Statement "}"
    fn parse_function(&mut self) -> ReExpNode<'a> {
        let start = self.current;
        match_token_or_err!(self, TokenType::Pipe)?;
        let input = self.parse_expr()?;
        match_token_or_err!(self, TokenType::Pipe)?;
        let output = if match_token!(self, TokenType::Arrow).is_err() {
            None
        } else {
            Some(Box::new(self.parse_expr()?))
        };
        let statements = self.parse_statement()?;
        let input = Box::new(input);
        let statements = Box::new(statements);
        let expr = Expression::Function {
            input,
            output,
            statements,
        };
        Ok(self.make_expr_node(expr, start))
    }

    fn make_stat_node(&self, stmt: Stat<'a>, start: usize) -> StatNode<'a> {
        let slice = self.tokens.slice(start, self.current);
        stmt.to_node(slice)
    }

    fn with_expr_tracking<F>(&mut self, f: F) -> ReExpNode<'a>
    where
        F: FnOnce(&mut Self) -> Result<Exp<'a>, ParserError>,
    {
        let start = self.current;
        let expr = f(self)?;
        Ok(self.make_expr_node(expr, start))
    }

    fn with_stat_tracking<F>(&mut self, f: F) -> ReStatNode<'a>
    where
        F: FnOnce(&mut Self) -> Result<Stat<'a>, ParserError>,
    {
        let start = self.current;
        let stmt = f(self)?;
        Ok(self.make_stat_node(stmt, start))
    }

    // Logical OR: expr || expr
    fn parse_logical_or(&mut self) -> ReExpNode<'a> {
        let start = self.current;
        let mut node = self.parse_logical_and()?;
        while match_token!(self, TokenType::Keyword(Or)).is_ok() {
            let operator = BinaryOperator::Or;
            let right = self.parse_logical_and()?;
            let expr = Expression::binary(node, operator, right);
            node = self.make_expr_node(expr, start);
        }
        Ok(node)
    }

    // Type Union: type | type
    fn parse_type_union(&mut self) -> ReExpNode<'a> {
        let start = self.current;
        let mut types = vec![self.parse_logical_or()?];
        while match_token!(self, TokenType::Pipe).is_ok() {
            types.push(self.parse_logical_or()?);
        }
        if types.len() == 1 {
            Ok(types.into_iter().next().unwrap())
        } else {
            let expr = Expression::sum(types);
            Ok(self.make_expr_node(expr, start))
        }
    }
    pub fn parse_statement_block(&mut self) -> Result<StatBlock<'a>, ParserError> {
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
    pub fn parse_statement(&mut self) -> ReStatNode<'a> {
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
                while let Ok(x) = self.parse_expr() {
                    expr_vec.push(x);
                    if match_token!(self, TokenType::Comma).is_err() {
                        break;
                    }
                }
                let _ = match_token_or_err!(self, TokenType::RightParen)?;
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
                    return Err(StatementParseError::BreakOutsideLoop.into());
                }
                self.advance();
                let stmt = Statement::Break;
                Ok(self.make_stat_node(stmt, start))
            }
            TokenType::Keyword(Keyword::Continue) => {
                if !self.is_in_loop {
                    return Err(StatementParseError::ContinueOutsideLoop.into());
                }
                self.advance();
                let stmt = Statement::Continue;
                Ok(self.make_stat_node(stmt, start))
            }
            _ => Err(StatementParseError::NoStatement.into()),
        }
    }
    // Logical AND: expr && expr
    fn parse_logical_and(&mut self) -> ReExpNode<'a> {
        let start = self.current;
        let mut node = self.parse_equality()?;
        while match_token!(self, TokenType::Keyword(And)).is_ok() {
            let operator = BinaryOperator::And;
            let right = self.parse_equality()?;
            let expr = Expression::binary(node, operator, right);
            node = self.make_expr_node(expr, start);
        }
        Ok(node)
    }

    // Equality: expr == expr, expr != expr
    fn parse_equality(&mut self) -> ReExpNode<'a> {
        let start = self.current;
        let mut node = self.parse_comparison()?;
        while let Ok(op) = match_token!(self, TokenType::EqualEqual | TokenType::BangEqual) {
            let operator = match op {
                TokenType::EqualEqual => BinaryOperator::Equal,
                TokenType::BangEqual => BinaryOperator::NotEqual,
                _ => unreachable!(),
            };
            let right = self.parse_comparison()?;
            let expr = Expression::binary(node, operator, right);
            node = self.make_expr_node(expr, start);
        }
        Ok(node)
    }

    // Comparison: < > <= >=
    fn parse_comparison(&mut self) -> ReExpNode<'a> {
        let start = self.current;
        let mut node = self.parse_expr()?;
        while let Ok(op) = match_token!(
            self,
            TokenType::Less | TokenType::LessEqual | TokenType::Greater | TokenType::GreaterEqual
        ) {
            let operator = match op {
                TokenType::Less => BinaryOperator::Less,
                TokenType::LessEqual => BinaryOperator::LessEqual,
                TokenType::Greater => BinaryOperator::Greater,
                TokenType::GreaterEqual => BinaryOperator::GreaterEqual,
                _ => unreachable!(),
            };
            let right = self.parse_expr()?;
            let expr = Expression::binary(node, operator, right);
            node = self.make_expr_node(expr, start);
        }
        Ok(node)
    }
    pub fn new(tokens: &'a TokenSlice<'a>) -> Self {
        Parser {
            tokens,
            current: 0,
            is_error: false,
            is_in_loop: false,
        }
    }

    pub fn parse_expression(&mut self) -> ReExpNode<'a> {
        self.parse_type_union()
    }

    // Expr ::= Term (("+" | "-") Term)*
    fn parse_expr(&mut self) -> ReExpNode<'a> {
        let start = self.current;
        let mut node = self.parse_term()?;
        while let Ok(op) = match_token!(self, TokenType::Plus | TokenType::Minus) {
            let operator = match op {
                TokenType::Plus => BinaryOperator::Plus,
                TokenType::Minus => BinaryOperator::Minus,
                _ => unreachable!(),
            };
            let right = self.parse_term()?;
            let expr = Expression::binary(node, operator, right);
            node = self.make_expr_node(expr, start);
        }
        Ok(node)
    }

    // Term ::= Unary (("*" | "/") Unary)*
    fn parse_term(&mut self) -> ReExpNode<'a> {
        let start = self.current;
        let mut node = self.parse_unary()?;
        while let Ok(op) = match_token!(self, TokenType::Star | TokenType::Slash) {
            let operator = match op {
                TokenType::Star => BinaryOperator::Multiply,
                TokenType::Slash => BinaryOperator::Divide,
                _ => unreachable!(),
            };
            let right = self.parse_unary()?;
            let expr = Expression::binary(node, operator, right);
            node = self.make_expr_node(expr, start);
        }
        Ok(node)
    }

    // Unary ::= ("+" | "-") Unary | MemberAccess
    fn parse_unary(&mut self) -> ReExpNode<'a> {
        let start = self.current;
        let operator = match self.peek_type().ok_or(ParserError::NO_EXPN_FOUND)? {
            TokenType::Minus => UnaryOperator::Minus,
            TokenType::Keyword(Keyword::Not) => UnaryOperator::Not,
            _ => {
                return self.parse_member_access();
            }
        };
        self.advance();
        let operand = self.parse_unary()?;
        let expr = Expression::unary(operator, operand);
        Ok(self.make_expr_node(expr, start))
    }

    // MemberAccess ::= Primary ("." Identifier)*
    fn parse_member_access(&mut self) -> ReExpNode<'a> {
        let start = self.current;
        let mut node = self.parse_primary()?;

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
    // Primary ::= Integer | Float | Identifier | "(" Expr ")" | Function
    fn parse_primary(&mut self) -> ReExpNode<'a> {
        let start = self.current;
        match self.peek_type().ok_or(ParserError::NO_EXPN_FOUND)? {
            TokenType::Pipe => self.parse_function(),
            TokenType::LeftBrace => self.parse_struct(),
            TokenType::LeftParen => {
                self.advance(); // consume '('
                let expr = self.parse_expression()?;
                if let Some(TokenType::RightParen) = self.peek_type() {
                    self.advance(); // consume ')'
                    Ok(self.make_expr_node(Expression::grouping(expr), start))
                } else {
                    Err(ParserError::expected_token_in_expression(
                        TokenType::RightParen,
                    ))
                }
            }
            any => {
                let expression =
                    Expression::from_token_type(any.clone()).ok_or(ParserError::NO_EXPN_FOUND)?;
                self.advance();
                Ok(self.make_expr_node(expression, start))
            }
        }
    }

    /// Returns the next token (peek), or None if at end.
    fn peek_type(&self) -> Option<&TokenType> {
        self.peek().map(|x| &x.token_type)
    }

    fn check(&self, tt: &TokenType) -> bool {
        Some(tt) == self.peek_type()
    }

    // Struct ::= "{" (Identifier "=" Expression ("," Identifier "=" Expression)*)? "}"
    fn parse_struct(&mut self) -> ReExpNode<'a> {
        let start = self.current;
        self.advance(); // consume '{'
        let mut fields = std::collections::HashMap::new();
        // Handle empty struct case
        if match_token!(self, TokenType::RightBrace).is_ok() {
            let expr = Expression::product(fields);
            return Ok(self.make_expr_node(expr, start));
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
                    return Err(ParserError::expected_token_in_expression(
                        TokenType::RightBrace,
                    ));
                }
            }
        }
        let expr = Expression::product(fields);
        Ok(self.make_expr_node(expr, start))
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

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }
}
