use crate::ast::{BinaryOperator, Expression, ExpressionNode, UnaryOperator};
use crate::token::token_type::Keyword::{And, Or};
use crate::token::{Token, TokenType};

pub struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
}

impl<'a> Parser<'a> {
    // Logical OR: expr || expr
    fn parse_logical_or(&mut self) -> Option<ExpressionNode> {
        let mut node = self.parse_logical_and()?;
        while self.match_token(&[TokenType::Keyword(Or)]).is_some() {
            let operator = BinaryOperator::Or;
            let right = self.parse_logical_and()?;
            node = Expression::binary(node, operator, right).into();
        }
        Some(node)
    }

    // Logical AND: expr && expr
    fn parse_logical_and(&mut self) -> Option<ExpressionNode> {
        let mut node = self.parse_equality()?;
        while self.match_token(&[TokenType::Keyword(And)]).is_some() {
            let operator = BinaryOperator::And;
            let right = self.parse_equality()?;
            node = Expression::binary(node, operator, right).into();
        }
        Some(node)
    }

    // Equality: expr == expr, expr != expr
    fn parse_equality(&mut self) -> Option<ExpressionNode> {
        let mut node = self.parse_comparison()?;
        while let Some(op) = self.match_token(&[TokenType::EqualEqual, TokenType::BangEqual]) {
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
    fn parse_comparison(&mut self) -> Option<ExpressionNode> {
        let mut node = self.parse_expr()?;
        while let Some(op) = self.match_token(&[
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
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser { tokens, current: 0 }
    }

    pub fn parse_expression(&mut self) -> Option<ExpressionNode> {
        self.parse_logical_or()
    }

    // Expr ::= Term (("+" | "-") Term)*
    fn parse_expr(&mut self) -> Option<ExpressionNode> {
        let mut node = self.parse_term()?;
        while let Some(op) = self.match_token(&[TokenType::Plus, TokenType::Minus]) {
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
    fn parse_term(&mut self) -> Option<ExpressionNode> {
        let mut node = self.parse_unary()?;
        while let Some(op) = self.match_token(&[TokenType::Star, TokenType::Slash]) {
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
    fn parse_unary(&mut self) -> Option<ExpressionNode> {
        if let Some(op) = self.match_token(&[TokenType::Minus, TokenType::Bang]) {
            let operator = match op {
                TokenType::Minus => UnaryOperator::Minus,
                TokenType::Bang => UnaryOperator::Not,
                _ => unreachable!(),
            };
            let operand = self.parse_unary()?;
            return Some(Expression::unary(operator, operand).into());
        }
        self.parse_primary()
    }

    // Primary ::= Integer | Float | Identifier | "(" Expr ")"
    fn parse_primary(&mut self) -> Option<ExpressionNode> {
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
        if self.is_at_end() {
            None
        } else {
            Some(&self.tokens[self.current].token_type)
        }
    }

    /// Returns the nth token ahead, or None if out of bounds.
    fn peek_type_nth(&self, n: usize) -> Option<&TokenType> {
        let idx = self.current + n;
        if idx < self.tokens.len() {
            Some(&self.tokens[idx].token_type)
        } else {
            None
        }
    }
    fn match_token(&mut self, types: &[TokenType]) -> Option<TokenType> {
        for tt in types.iter() {
            if self.check(tt) {
                self.advance();
                return Some(tt.clone());
            }
        }
        None
    }

    fn consume(&mut self, tt: &TokenType) -> Option<&Token> {
        if self.check(tt) {
            return Some(self.advance());
        }
        None
    }

    fn check(&self, tt: &TokenType) -> bool {
        !self.is_at_end() && self.peek().token_type == *tt
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

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
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
    fn test_parse_addition_with_tokenizer() {
        // 1 + 2
        let source = "1 + 2";
        let tokenizer = Tokenizer::new(source);
        let tokens = tokenizer.tokenize();
        let mut parser = Parser::new(&tokens);
        let parsed = parser.parse_expression().unwrap();
        let expected = Expression::binary(
            Expression::integer(1),
            BinaryOperator::Plus,
            Expression::integer(2),
        )
        .into();
        assert_eq!(parsed, expected);
    }

    #[test]
    fn test_parse_subtraction() {
        // 1 - 2
        let source = "1 - 2";
        let tokenizer = Tokenizer::new(source);
        let tokens = tokenizer.tokenize();
        let mut parser = Parser::new(&tokens);
        let parsed = parser.parse_expression().unwrap();
        let expected = Expression::binary(
            Expression::integer(1),
            BinaryOperator::Minus,
            Expression::integer(2),
        )
        .into();
        assert_eq!(parsed, expected);
    }

    #[test]
    fn test_parse_multiplication() {
        // 2 * 3
        let source = "2 * 3";
        let tokenizer = Tokenizer::new(source);
        let tokens = tokenizer.tokenize();
        let mut parser = Parser::new(&tokens);
        let parsed = parser.parse_expression().unwrap();
        let expected = Expression::binary(
            Expression::integer(2),
            BinaryOperator::Multiply,
            Expression::integer(3),
        )
        .into();
        assert_eq!(parsed, expected);
    }

    #[test]
    fn test_parse_division() {
        // 4 / 2
        let source = "4 / 2";
        let tokenizer = Tokenizer::new(source);
        let tokens = tokenizer.tokenize();
        let mut parser = Parser::new(&tokens);
        let parsed = parser.parse_expression().unwrap();
        let expected = Expression::binary(
            Expression::integer(4),
            BinaryOperator::Divide,
            Expression::integer(2),
        )
        .into();
        assert_eq!(parsed, expected);
    }

    #[test]
    fn test_parse_unary_minus() {
        // -5
        let source = "-5";
        let tokenizer = Tokenizer::new(source);
        let tokens = tokenizer.tokenize();
        let mut parser = Parser::new(&tokens);
        let parsed = parser.parse_expression().unwrap();
        let expected = Expression::unary(UnaryOperator::Minus, Expression::integer(5)).into();
        assert_eq!(parsed, expected);
    }

    #[test]
    fn test_parse_grouping_and_precedence() {
        // (1 + 2) * 3
        let source = "(1 + 2) * 3";
        let tokenizer = Tokenizer::new(source);
        let tokens = tokenizer.tokenize();
        let mut parser = Parser::new(&tokens);
        let parsed = parser.parse_expression().unwrap();
        let expected = Expression::binary(
            Expression::grouping(Expression::binary(
                Expression::integer(1),
                BinaryOperator::Plus,
                Expression::integer(2),
            )),
            BinaryOperator::Multiply,
            Expression::integer(3),
        )
        .into();
        assert_eq!(parsed, expected);
    }

    #[test]
    fn test_parse_mixed_precedence() {
        // 1 + 2 * 3
        let source = "1 + 2 * 3";
        let tokenizer = Tokenizer::new(source);
        let tokens = tokenizer.tokenize();
        let mut parser = Parser::new(&tokens);
        let parsed = parser.parse_expression().unwrap();
        let expected = Expression::binary(
            Expression::integer(1),
            BinaryOperator::Plus,
            Expression::binary(
                Expression::integer(2),
                BinaryOperator::Multiply,
                Expression::integer(3),
            ),
        )
        .into();
        assert_eq!(parsed, expected);
    }

    #[test]
    fn test_parse_float_literal() {
        // 3.14
        let source = "3.1";
        let tokenizer = Tokenizer::new(source);
        let tokens = tokenizer.tokenize();
        let mut parser = Parser::new(&tokens);
        let parsed = parser.parse_expression().unwrap();
        let expected = Expression::float(3.1).into();
        assert_eq!(parsed, expected);
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
