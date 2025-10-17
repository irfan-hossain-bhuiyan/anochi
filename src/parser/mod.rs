mod parser_error;
use parser_error::*;
use crate::ast::{
     BinaryOperator,  Expression, ExpressionNode, Statement, StatementNode, UnaryOperator,
};
use crate::token::token_type::Keyword::{self, And, Or};
use crate::token::{Token, TokenType};
pub struct Parser<'a, 'b: 'a> {
    tokens: &'a [Token<'b>],
    current: usize,
    is_error: bool,
}
type ExpNode<'a> = ExpressionNode<'a>;
type ReExpNode<'a> = Result<ExpNode<'a>,ParserError>;
type StatNode<'a> = StatementNode<'a>;
type ReStatNode<'a> = Result<StatementNode<'a>,ParserError>;
type MatchTokenResult = Result<&'static TokenType,NoTokenFound>;
impl<'a, 'b: 'a> Parser<'a, 'b> {
    // Logical OR: expr || expr
    fn parse_logical_or(&mut self) -> ReExpNode<'b> {
        let mut node = self.parse_logical_and()?;
        while self.match_token(&TokenType::Keyword(Or)).is_ok() {
            let operator = BinaryOperator::Or;
            let right = self.parse_logical_and()?;
            node = Expression::binary(node, operator, right).into();
        }
        Ok(node)
    }
    pub fn parse_statement(&mut self) -> ReStatNode<'b> {
        // Assignment: identifier = expression
        match self.peek_type().unwrap().clone() {
            TokenType::Identifier(x) => {
                self.advance();
                let _ = self.match_token(&TokenType::Equal).map_err(|x|x.to_parser_error())?;
                let expr = self.parse_expression()?;
                let _ = self.match_token(&TokenType::Semicolon).map_err(|x|x.to_parser_error())?;
                Ok(Statement::assignment(x.to_string(), expr).into())
            }
            TokenType::LeftBrace => {
                self.advance();
                let mut statements = Vec::new();
                loop {
                    if self.match_token(&TokenType::RightBrace).is_ok(){
                        break;
                    }
                    match self.parse_statement() {
                        Ok(x)=>statements.push(x),
                        err @ Err(_)=>return err,
                    }
                }
                Ok(Statement::statement_block(statements).into())
            }
            TokenType::Keyword(Keyword::If) => {
                self.advance();
                let expr = self.parse_expression()?;
                let on_true = self.parse_statement()?;
                match self.match_token(&TokenType::Keyword(Keyword::Else)) {
                    Err(_) => Ok(Statement::if_stmt(expr, on_true).into()),
                    Ok(_) => {
                        let on_false = self.parse_statement()?;
                        Ok(Statement::if_else(expr, on_true, on_false).into())
                    }
                }
            }
            _=>{
                Err(StatementParseError::NoStatement.into())
            }
        }
    }
    // Logical AND: expr && expr
    fn parse_logical_and(&mut self) -> ReExpNode<'b> {
        let mut node = self.parse_equality()?;
        while self.match_token(&TokenType::Keyword(And)).is_ok() {
            let operator = BinaryOperator::And;
            let right = self.parse_equality()?;
            node = Expression::binary(node, operator, right).into();
        }
        Ok(node)
    }

    // Equality: expr == expr, expr != expr
    fn parse_equality(&mut self) -> ReExpNode<'b> {
        let mut node = self.parse_comparison()?;
        while let Ok(op) = self.match_tokens(&[TokenType::EqualEqual, TokenType::BangEqual]) {
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
        while let Ok(op) = self.match_tokens(&[
            TokenType::Less,
            TokenType::LessEqual,
            TokenType::Greater,
            TokenType::GreaterEqual,
        ]) {
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
        Parser { tokens, current: 0,is_error:false }
    }

    pub fn parse_expression(&mut self) -> ReExpNode<'b> {
        self.parse_logical_or()
    }

    // Expr ::= Term (("+" | "-") Term)*
    fn parse_expr(&mut self) -> ReExpNode<'b> {
        let mut node = self.parse_term()?;
        while let Ok(op) = self.match_tokens(&[TokenType::Plus, TokenType::Minus]) {
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
        while let Ok(op) = self.match_tokens(&[TokenType::Star, TokenType::Slash]) {
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
        let operator = match self.peek_type().ok_or(ParserError::NO_EXPN_FOUND)?{
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
            TokenType::LeftParen => {
                self.advance(); // consume '('
                let expr = self.parse_expression()?;
                if let Some(TokenType::RightParen) = self.peek_type() {
                    self.advance(); // consume ')'
                    return Ok(Expression::grouping(expr).into());
                } else{
                    Err(ParserError::expected_token_in_expression(TokenType::RightParen))
                }
            }
            any => {
                let expression = Expression::from_token_type(any.clone()).ok_or(ParserError::NO_EXPN_FOUND)?;
                self.advance();
                Ok(expression.into())
            }
        }
    }

    /// Returns the next token (peek), or None if at end.
    fn peek_type(&self) -> Option<&TokenType> {
        self.peek().map(|x| &x.token_type)
    }

    fn match_tokens(&mut self, types: &'static [TokenType]) ->MatchTokenResult {
        let Some(current_token_type)=self.peek_type() else{return Err(NoTokenFound::NoneToken);};
        for tt in types.iter() {
            if tt==current_token_type{
                self.advance();
                return Ok(tt)
            }
        }
        Err(NoTokenFound::new(current_token_type.clone()))
        
    }
    fn match_token(&mut self, r#type: &'static TokenType) -> MatchTokenResult {
        let Some(current_token_type)= self.peek_type() else{return Err(NoTokenFound::NoneToken)};
        if current_token_type==r#type {
            self.advance();
            return Ok(r#type);
        }
        Err(NoTokenFound::new(current_token_type.clone()))

    }
    fn check(&self, tt: &TokenType) -> bool {
        Some(tt) == self.peek_type()
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinaryOperator, Expression};

    use crate::token::Tokenizer;

    #[test]
    fn test_parse_assignment_statement() {
        // x = 42;
        let source = "x = 42;";
        let tokenizer = Tokenizer::new(source);
        let tokens = tokenizer.tokenize();
        let mut parser = Parser::new(&tokens);
        let parsed = parser.parse_statement().unwrap();
        let expected = Statement::assignment("x".to_string(), Expression::integer(42)).into();
        assert_eq!(parsed, expected);
    }

    #[test]
    fn test_parse_statement_block() {
        // (x = 1; y = 2;)
        let source = "{x = 1; y = 2;}";
        let tokenizer = Tokenizer::new(source);
        let tokens = tokenizer.tokenize();
        let mut parser = Parser::new(&tokens);
        let parsed = parser.parse_statement().unwrap();
        let expected = Statement::statement_block(vec![
            Statement::assignment("x".to_string(), Expression::integer(1)).into(),
            Statement::assignment("y".to_string(), Expression::integer(2)).into(),
        ])
        .into();
        assert_eq!(parsed, expected);
    }

    #[test]
    fn test_parse_if_comparison_block() {
        // if (2 > 1) { x = 42; }
        let source = "if (2 > 1) { x = 42; }";
        let tokenizer = Tokenizer::new(source);
        let tokens = tokenizer.tokenize();
        let mut parser = Parser::new(&tokens);
        let parsed = parser.parse_statement().unwrap();
        let expected = Statement::if_stmt(
            Expression::grouping(Expression::binary(
                Expression::integer(2),
                BinaryOperator::Greater,
                Expression::integer(1),
            )),
            Statement::statement_block(vec![
                Statement::assignment("x".to_string(), Expression::integer(42)).into(),
            ]),
        )
        .into();
        assert_eq!(parsed, expected);
    }

    #[test]
    fn test_parse_if_else_statement() {
        // if 1 x = 2; else x = 3;
        let source = "if 1 x = 2; else x = 3;";
        let tokenizer = Tokenizer::new(source);
        let tokens = tokenizer.tokenize();
        let mut parser = Parser::new(&tokens);
        let parsed = parser.parse_statement().unwrap();
        let expected = Statement::if_else(
            Expression::integer(1),
            Statement::assignment("x".to_string(), Expression::integer(2)),
            Statement::assignment("x".to_string(), Expression::integer(3)),
        )
        .into();
        assert_eq!(parsed, expected);
    }
    #[test]
    fn test_comprehensive_expression_parsing() {
        // Test arithmetic operators and precedence in one comprehensive test
        let test_cases = vec![
            // Basic arithmetic
            ("1 + 2", Expression::binary(Expression::integer(1), BinaryOperator::Plus, Expression::integer(2))),
            ("4 / 2", Expression::binary(Expression::integer(4), BinaryOperator::Divide, Expression::integer(2))),
            // Unary operations
            ("-5", Expression::unary(UnaryOperator::Minus, Expression::integer(5))),
            // Float literals
            ("3.1", Expression::float(3.1)),
            // Precedence: multiplication before addition
            ("1 + 2 * 3", Expression::binary(
                Expression::integer(1),
                BinaryOperator::Plus,
                Expression::binary(Expression::integer(2), BinaryOperator::Multiply, Expression::integer(3))
            )),
            // Grouping overrides precedence
            ("(1 + 2) * 3", Expression::binary(
                Expression::grouping(Expression::binary(Expression::integer(1), BinaryOperator::Plus, Expression::integer(2))),
                BinaryOperator::Multiply,
                Expression::integer(3)
            )),
        ];

        for (source, expected_expr) in test_cases {
            let tokenizer = Tokenizer::new(source);
            let tokens = tokenizer.tokenize();
            let mut parser = Parser::new(&tokens);
            let parsed = parser.parse_expression().unwrap();
            let expected = expected_expr.into();
            assert_eq!(parsed, expected, "Failed parsing: {}", source);
        }
    }

    #[test]
    fn test_parse_nested_brackets_expression() {
        // ((1 + 2) * (3 - 4))
        let source = "((1 + 2) * (3 - 4))";
        let tokenizer = Tokenizer::new(source);
        let tokens = tokenizer.tokenize();
        let mut parser = Parser::new(&tokens);
        let parsed = parser.parse_expression().unwrap();
        let expected = Expression::grouping(Expression::binary(
            Expression::grouping(Expression::binary(
                Expression::integer(1),
                BinaryOperator::Plus,
                Expression::integer(2),
            )),
            BinaryOperator::Multiply,
            Expression::grouping(Expression::binary(
                Expression::integer(3),
                BinaryOperator::Minus,
                Expression::integer(4),
            )),
        ))
        .into();
        assert_eq!(parsed, expected);
    }
}
