mod parser_error;
use crate::ast::{
    BinaryOperator, Expression, ExpressionNode, Statement, StatementNode, UnaryOperator,
};
use crate::token::token_type::Keyword::{self, And, Or};
use crate::token::{Token, TokenType};
use parser_error::*;

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
            None => Err(NoTokenFound::NoneToken),
        }
    }};
}

/// Macro for token matching with error conversion to ParserError
macro_rules! match_token_or_err {
    ($self:expr, $pattern:pat) => {{
        match_token!($self, $pattern).map_err(|x| x.to_parser_error())
    }};
}

pub struct Parser<'a, 'b: 'a> {
    tokens: &'a [Token<'b>],
    current: usize,
    is_error: bool,
}
type ExpNode<'a> = ExpressionNode<'a>;
type ReExpNode<'a> = Result<ExpNode<'a>, ParserError>;
type StatNode<'a> = StatementNode<'a>;
type ReStatNode<'a> = Result<StatementNode<'a>, ParserError>;
impl<'a, 'b: 'a> Parser<'a, 'b> {
    // Logical OR: expr || expr
    fn parse_logical_or(&mut self) -> ReExpNode<'b> {
        let mut node = self.parse_logical_and()?;
        while match_token!(self, TokenType::Keyword(Or)).is_ok() {
            let operator = BinaryOperator::Or;
            let right = self.parse_logical_and()?;
            node = Expression::binary(node, operator, right).into();
        }
        Ok(node)
    }
   
   // Type Union: type | type
   fn parse_type_union(&mut self) -> ReExpNode<'b> {
       let mut types = vec![self.parse_logical_or()?];
       while match_token!(self, TokenType::Pipe).is_ok() {
           types.push(self.parse_logical_or()?);
       }
       if types.len() == 1 {
           Ok(types.into_iter().next().unwrap())
       } else {
           Ok(Expression::sum(types).into())
       }
   }

    pub fn parse_statement(&mut self) -> ReStatNode<'b> {
        // Assignment: identifier = expression
        match self.peek_type().unwrap().clone() {
            TokenType::Identifier(_) => {
                let x=self.parse_expression()?;
                // Check for typed assignment: identifier : type = value
                if match_token!(self, TokenType::Colon).is_ok() {
                    let type_expr = self.parse_expression()?;
                    let _ = match_token_or_err!(self, TokenType::Equal)?;
                    let value_expr = self.parse_expression()?;
                    let _ = match_token_or_err!(self, TokenType::Semicolon)?;
                    Ok(Statement::assignment(x, value_expr, type_expr).into())
                } else {
                    // Regular assignment: identifier = value
                    let _ = match_token_or_err!(self, TokenType::Equal)?;
                    let expr = self.parse_expression()?;
                    let _ = match_token_or_err!(self, TokenType::Semicolon)?;
                    Ok(Statement::assignment_unknowntype(x, expr).into())
                }
            }
            TokenType::LeftBrace => {
                self.advance();
                let mut statements = Vec::new();
                loop {
                    if match_token!(self, TokenType::RightBrace).is_ok() {
                        break;
                    }
                    match self.parse_statement() {
                        Ok(x) => statements.push(x),
                        err @ Err(_) => return err,
                    }
                }
                Ok(Statement::statement_block(statements).into())
            }
            TokenType::Keyword(Keyword::If) => {
                self.advance();
                let expr = self.parse_expression()?;
                let on_true = self.parse_statement()?;
                match match_token!(self, TokenType::Keyword(Keyword::Else)) {
                    Err(_) => Ok(Statement::if_stmt(expr, on_true).into()),
                    Ok(_) => {
                        let on_false = self.parse_statement()?;
                        Ok(Statement::if_else(expr, on_true, on_false).into())
                    }
                }
            }
            TokenType::Keyword(Keyword::Debug) => {
                self.advance();
                let _=match_token_or_err!(self, TokenType::LeftParen)?;
                let mut expr_vec=Vec::new();
                while let Ok(x)=self.parse_expr(){
                    expr_vec.push(x);
                    if match_token!(self, TokenType::Comma).is_err(){
                        break;
                    }
                }
                let _=match_token_or_err!(self, TokenType::RightParen)?;
                Ok(Statement::debug(expr_vec).into())
            }
            _ => Err(StatementParseError::NoStatement.into()),
        }
    }
    // Logical AND: expr && expr
    fn parse_logical_and(&mut self) -> ReExpNode<'b> {
        let mut node = self.parse_equality()?;
        while match_token!(self, TokenType::Keyword(And)).is_ok() {
            let operator = BinaryOperator::And;
            let right = self.parse_equality()?;
            node = Expression::binary(node, operator, right).into();
        }
        Ok(node)
    }

    // Equality: expr == expr, expr != expr
    fn parse_equality(&mut self) -> ReExpNode<'b> {
        let mut node = self.parse_comparison()?;
        while let Ok(op) = match_token!(self, TokenType::EqualEqual | TokenType::BangEqual) {
            let operator = match op {
                TokenType::EqualEqual => BinaryOperator::Equal,
                TokenType::BangEqual => BinaryOperator::NotEqual,
                _ => unreachable!(),
            };
            let right = self.parse_comparison()?;
            node = Expression::binary(node, operator, right).into();
        }
        Ok(node)
    }

    // Comparison: < > <= >=
    fn parse_comparison(&mut self) -> ReExpNode<'b> {
        let mut node = self.parse_expr()?;
        while let Ok(op) = match_token!(self, TokenType::Less | TokenType::LessEqual | TokenType::Greater | TokenType::GreaterEqual) {
            let operator = match op {
                TokenType::Less => BinaryOperator::Less,
                TokenType::LessEqual => BinaryOperator::LessEqual,
                TokenType::Greater => BinaryOperator::Greater,
                TokenType::GreaterEqual => BinaryOperator::GreaterEqual,
                _ => unreachable!(),
            };
            let right = self.parse_expr()?;
            node = Expression::binary(node, operator, right).into();
        }
        Ok(node)
    }
    pub fn new(tokens: &'a [Token<'b>]) -> Self {
        Parser {
            tokens,
            current: 0,
            is_error: false,
        }
    }

    pub fn parse_expression(&mut self) -> ReExpNode<'b> {
        self.parse_type_union()
    }

    // Expr ::= Term (("+" | "-") Term)*
    fn parse_expr(&mut self) -> ReExpNode<'b> {
        let mut node = self.parse_term()?;
        while let Ok(op) = match_token!(self, TokenType::Plus | TokenType::Minus) {
            let operator = match op {
                TokenType::Plus => BinaryOperator::Plus,
                TokenType::Minus => BinaryOperator::Minus,
                _ => unreachable!(),
            };
            let right = self.parse_term()?;
            node = Expression::binary(node, operator, right).into();
        }
        Ok(node)
    }

    // Term ::= Unary (("*" | "/") Unary)*
    fn parse_term(&mut self) -> ReExpNode<'b> {
        let mut node = self.parse_unary()?;
        while let Ok(op) = match_token!(self, TokenType::Star | TokenType::Slash) {
            let operator = match op {
                TokenType::Star => BinaryOperator::Multiply,
                TokenType::Slash => BinaryOperator::Divide,
                _ => unreachable!(),
            };
            let right = self.parse_unary()?;
            node = Expression::binary(node, operator, right).into();
        }
        Ok(node)
    }

    // Unary ::= ("+" | "-") Unary | Primary
    fn parse_unary(&mut self) -> ReExpNode<'b> {
        let operator = match self.peek_type().ok_or(ParserError::NO_EXPN_FOUND)? {
            TokenType::Minus => UnaryOperator::Minus,
            TokenType::Keyword(Keyword::Not) => UnaryOperator::Not,
            _ => {
                return self.parse_primary();
            }
        };
        self.advance();
        let operand = self.parse_unary()?;
        Ok(Expression::unary(operator, operand).into())
    }
    // Primary ::= Integer | Float | Identifier | "(" Expr ")"
    fn parse_primary(&mut self) -> ReExpNode<'b> {
        match self.peek_type().ok_or(ParserError::NO_EXPN_FOUND)? {
            TokenType::LeftBrace => {
                self.parse_struct()
            }
            TokenType::LeftParen => {
                self.advance(); // consume '('
                let expr = self.parse_expression()?;
                if let Some(TokenType::RightParen) = self.peek_type() {
                    self.advance(); // consume ')'
                    Ok(Expression::grouping(expr).into())
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
                Ok(expression.into())
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
    fn parse_struct(&mut self) -> ReExpNode<'b> {
        self.advance(); // consume '{'
        
        let mut fields = std::collections::HashMap::new();
        
        // Handle empty struct case
        if match_token!(self, TokenType::RightBrace).is_ok() {
            return Ok(Expression::product(fields).into());
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
                Ok(TokenType::Comma) => continue, // More fields to parse
                Ok(TokenType::RightBrace) => break, // End of struct
                Ok(_) => unreachable!(), // Should never happen due to pattern
                Err(_) => return Err(ParserError::expected_token_in_expression(TokenType::RightBrace)),
            }
        }
        
        Ok(Expression::product(fields).into())
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

