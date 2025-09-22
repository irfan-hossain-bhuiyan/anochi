mod token;
mod ast;

use display_tree::{format_tree, CharSet, StyleBuilder,Style};
use ast::{Expression, BinaryOperator, UnaryOperator};

fn main() {
    // Create an AST representing the expression: (-2 + 7)
    let expr = Expression::binary(
        Expression::unary(UnaryOperator::Minus, Expression::integer(2)),
        BinaryOperator::Plus,
        Expression::integer(7),
    );

    // Display the AST using the display_tree crate
    let formatted_tree = format_tree!(
        expr,
        Style::default().indentation(1).char_set(CharSet::DOUBLE_LINE)
    );

    println!("{formatted_tree}");
}
