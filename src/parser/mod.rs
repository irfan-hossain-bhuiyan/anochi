use crate::ast::{
    BinaryOperator, Expression, ExpressionNode, Statement, StatementNode, UnaryOperator,
};
use crate::token::token_type::Keyword::{self, And, Or};
use crate::token::{Token, TokenType};

pub struct Parser<'a, 'b: 'a> {
    tokens: &'a [Token<'b>],
    current: usize,
}
type ExpNode<'a> = ExpressionNode<'a>;
type OpExpNode<'a> = Option<ExpNode<'a>>;
type StatNode<'a> = StatementNode<'a>;
type OpStatNode<'a> = Option<StatementNode<'a>>;
impl<'a, 'b: 'a> Parser<'a, 'b> {
    // Logical OR: expr || expr
    fn parse_logical_or(&mut self) -> OpExpNode<'b> {
        let mut node = self.parse_logical_and()?;
        while self.match_tokens(&[TokenType::Keyword(Or)]).is_some() {
            let operator = BinaryOperator::Or;
            let right = self.parse_logical_and()?;
            node = Expression::binary(node, operator, right).into();
        }
        Some(node)
    }
    pub fn parse_statement(&mut self) -> OpStatNode<'b> {
        // Assignment: identifier = expression
        match self.peek_type()?.clone() {
            TokenType::Identifier(x) => {
                self.advance();
                let _ = self.match_token(&TokenType::Equal)?;
                let expr = self.parse_expression()?;
                let _ = self.match_token(&TokenType::Semicolon)?;
                Some(Statement::assignment(x.to_string(), expr).into())
            }
            TokenType::LeftBrace => {
                self.advance();
                let mut statements = Vec::new();
                while let Some(stmt) = self.parse_statement() {
                    statements.push(stmt);
                }
                let _ = self.match_token(&TokenType::RightBrace);
                Some(Statement::statement_block(statements).into())
            }
            TokenType::Keyword(Keyword::If) => {
                self.advance();
                let expr = self.parse_expression()?;
                let on_true = self.parse_statement()?;
                match self.match_token(&TokenType::Keyword(Keyword::Else)) {
                    None => Some(Statement::if_stmt(expr, on_true).into()),
                    Some(_) => {
                        let on_false = self.parse_statement()?;
                        Some(Statement::if_else(expr, on_true, on_false).into())
                    }
                }
            }
            _ => None,
        }
    }
    // Logical AND: expr && expr
    fn parse_logical_and(&mut self) -> OpExpNode<'b> {
        let mut node = self.parse_equality()?;
        while self.match_tokens(&[TokenType::Keyword(And)]).is_some() {
            let operator = BinaryOperator::And;
            let right = self.parse_equality()?;
            node = Expression::binary(node, operator, right).into();
        }
        Some(node)
    }

    // Equality: expr == expr, expr != expr
    fn parse_equality(&mut self) -> OpExpNode<'b> {
        let mut node = self.parse_comparison()?;
        while let Some(op) = self.match_tokens(&[TokenType::EqualEqual, TokenType::BangEqual]) {
            let operator = match op {
                TokenType::EqualEqual => BinaryOperator::Equal,
                TokenType::BangEqual => BinaryOperator::NotEqual,
                _ => unreachable!(),
            };
            let right = self.parse_comparison()?;
            node = Expression::binary(node, operator, right).into();
        }
        Some(node)
    }

    // Comparison: < > <= >=
    fn parse_comparison(&mut self) -> OpExpNode<'b> {
        let mut node = self.parse_expr()?;
        while let Some(op) = self.match_tokens(&[
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
        Some(node)
    }
    pub fn new(tokens: &'a [Token<'b>]) -> Self {
        Parser { tokens, current: 0 }
    }

    pub fn parse_expression(&mut self) -> OpExpNode<'b> {
        self.parse_logical_or()
    }

    // Expr ::= Term (("+" | "-") Term)*
    fn parse_expr(&mut self) -> OpExpNode<'b> {
        let mut node = self.parse_term()?;
        while let Some(op) = self.match_tokens(&[TokenType::Plus, TokenType::Minus]) {
            let operator = match op {
                TokenType::Plus => BinaryOperator::Plus,
                TokenType::Minus => BinaryOperator::Minus,
                _ => unreachable!(),
            };
            let right = self.parse_term()?;
            node = Expression::binary(node, operator, right).into();
        }
        Some(node)
    }

    // Term ::= Unary (("*" | "/") Unary)*
    fn parse_term(&mut self) -> OpExpNode<'b> {
        let mut node = self.parse_unary()?;
        while let Some(op) = self.match_tokens(&[TokenType::Star, TokenType::Slash]) {
            let operator = match op {
                TokenType::Star => BinaryOperator::Multiply,
                TokenType::Slash => BinaryOperator::Divide,
                _ => unreachable!(),
            };
            let right = self.parse_unary()?;
            node = Expression::binary(node, operator, right).into();
        }
        Some(node)
    }

    // Unary ::= ("+" | "-") Unary | Primary
    fn parse_unary(&mut self) -> OpExpNode<'b> {
        let operator = match self.peek_type()? {
            TokenType::Minus => UnaryOperator::Minus,
            TokenType::Keyword(Keyword::Not) => UnaryOperator::Not,
            _ => {
                return self.parse_primary();
            }
        };
        self.advance();
        let operand = self.parse_unary()?;
        Some(Expression::unary(operator, operand).into())
    }

    // Primary ::= Integer | Float | Identifier | "(" Expr ")"
    fn parse_primary(&mut self) -> OpExpNode<'b> {
        match self.peek_type()? {
            TokenType::LeftParen => {
                self.advance(); // consume '('
                let expr = self.parse_expression()?;
                if let Some(TokenType::RightParen) = self.peek_type() {
                    self.advance(); // consume ')'
                    Some(Expression::grouping(expr).into())
                } else {
                    // Error: expected ')'
                    None
                }
            }
            _ => {
                let expression = Expression::from_token_type(self.peek_type()?.clone())?;
                self.advance();
                Some(expression.into())
            }
        }
    }

    /// Returns the next token (peek), or None if at end.
    fn peek_type(&self) -> Option<&TokenType> {
        self.peek().map(|x| &x.token_type)
    }

    fn match_tokens(&mut self, types: &'static [TokenType]) -> Option<&'static TokenType> {
        for tt in types.iter() {
            if self.check(tt) {
                self.advance();
                return Some(tt);
            }
        }
        None
    }
    fn match_token(&mut self, r#type: &'static TokenType) -> Option<&'static TokenType> {
        if self.check(r#type) {
            self.advance();
            return Some(r#type);
        }
        None
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
