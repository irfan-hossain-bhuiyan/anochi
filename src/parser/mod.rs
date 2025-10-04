use crate::ast::{BinaryOperator, Expression, ExpressionNode, UnaryOperator};
use crate::token::{Token, TokenType};

pub struct Parser<'a> {
    tokens: &'a [Token],
    current: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Parser { tokens, current: 0 }
    }

    pub fn parse_expression(&mut self) -> Option<ExpressionNode> {
        self.parse_expr()
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
        if let Some(op) = self.match_token(&[TokenType::Minus]) {
            let operator = match op {
                TokenType::Minus => UnaryOperator::Minus,
                _ => unreachable!(),
            };
            let operand = self.parse_unary()?;
            return Some(Expression::unary(operator, operand).into());
        }
        self.parse_primary()
    }

    // Primary ::= Integer | Float | Identifier | "(" Expr ")"
    fn parse_primary(&mut self) -> Option<ExpressionNode> {
        if self.match_token(&[TokenType::Integer(0)]).is_some() {
            let token = self.previous();
            let value = match &token.token_type {
                TokenType::Integer(v) => *v,
                _ => return None,
            };
            return Some(Expression::integer(value).into());
        }
        if self.match_token(&[TokenType::Float(0.0)]).is_some() {
            let token = self.previous();
            let value = match &token.token_type {
                TokenType::Float(v) => *v,
                _ => return None,
            };
            return Some(Expression::float(value).into());
        }
        if self.match_token(&[TokenType::Identifier(String::new())]).is_some() {
            let token = self.previous();
            let name = match &token.token_type {
                TokenType::Identifier(s) => s.clone(),
                _ => return None,
            };
            return Some(Expression::identifier(name).into());
        }
        if self.match_token(&[TokenType::LeftParen]).is_some() {
            let expr = self.parse_expr()?;
            self.consume(&TokenType::RightParen)?;
            return Some(Expression::grouping(expr).into());
        }
        None
    }

    // Utility methods
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
    use crate::ast::{Expression, BinaryOperator};

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
            Expression::integer(2)
        ).into();
        assert_eq!(parsed, expected);
    }
}
