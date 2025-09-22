# Anochi Programming Language - Requirements Document

## Project Overview
Anochi is a compiled programming language implemented in Rust, designed to be productive while maintaining simplicity and performance.

## Core Architecture Requirements

### 1. Code Organization and Structure
- **Modular Design**: Split functionality into separate modules and files
- **Clear Separation of Concerns**: Each module should have a single, well-defined responsibility
- **Hierarchical Module Structure**: Use nested modules to organize related functionality

### 2. Rust Conventions and Best Practices
- **Naming Conventions**:
  - `snake_case` for functions, variables, and modules
  - `PascalCase` for types, structs, enums, and traits
  - `SCREAMING_SNAKE_CASE` for constants
  - Descriptive names that clearly indicate purpose
- **Error Handling**: Use `Result<T, E>` for fallible operations
- **Memory Safety**: Leverage Rust's ownership system, avoid unsafe code unless absolutely necessary
- **Performance**: Write idiomatic Rust code that compiles to efficient machine code

### 3. Documentation Requirements
- **Function Documentation**: Every public function must have rustdoc comments
- **Module Documentation**: Each module should have a module-level documentation
- **Example Code**: Include doctests in documentation where applicable
- **Integration Tests**: Comprehensive test coverage using `cargo test`
- **Documentation Tests**: Ensure all examples in documentation compile and run correctly

---

## Current AST Implementation

### Overview
The AST (Abstract Syntax Tree) module provides the data structures and functionality for representing parsed source code. The implementation focuses on expressions as the primary building blocks with support for:

- Binary expressions (arithmetic, comparison, logical operations)
- Unary expressions (negation, logical NOT)
- Literal expressions (integers, floats, strings, identifiers)
- Grouping expressions (parentheses for precedence)

### Core Data Structures

```rust
// Core literal types
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    String(String),
    Identifier(String),
}

// Binary operators
#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    // Arithmetic: +, -, *, /, %
    Plus, Minus, Multiply, Divide, Modulo,
    // Comparison: ==, !=, <, <=, >, >=
    Equal, NotEqual, Less, LessEqual, Greater, GreaterEqual,
    // Logical: &&, ||
    And, Or,
}

// Unary operators
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Minus, // -
    Not,   // !
}

// Main expression type
#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Literal(Literal),
    Binary { left: Box<Expression>, operator: BinaryOperator, right: Box<Expression> },
    Unary { operator: UnaryOperator, operand: Box<Expression> },
    Grouping { expression: Box<Expression> },
}
```

### Key Features

1. **Pretty Printing**: ASCII tree visualization for debugging and development
2. **Error Handling**: Structured errors using thiserror (DivisionByZero, TypeMismatch, etc.)
3. **Helper Methods**: Convenience constructors for common expressions
4. **Tree-walking Evaluator**: Basic interpreter implementation for debugging purposes only (not part of final compiled output)

### Simplified Test Suite

The test suite has been reduced to 4 essential tests:

1. **test_basic_operations**: Tests literal creation, binary expressions, and unary expressions
2. **test_expression_evaluation_with_type_coercion**: Tests mixed-type operations and complex expressions
3. **test_pretty_print_functionality**: Tests ASCII tree output for simple and nested expressions
4. **test_error_handling**: Tests division by zero and other error conditions

### Dependencies
- `thiserror = "1.0"` for structured error handling
- Uses `crate::token::Position` for source location tracking

### Usage Example

```rust
use anochi::ast::{Expression, BinaryOperator};

// Create expression: (2 + 3) * 4
let expr = Expression::binary(
    Expression::grouping(
        Expression::binary(
            Expression::integer(2),
            BinaryOperator::Plus,
            Expression::integer(3)
        )
    ),
    BinaryOperator::Multiply,
    Expression::integer(4)
);

// Evaluate: (2 + 3) * 4 = 20
let result = expr.evaluate().unwrap();

// Pretty print the tree structure
println!("{formatted_tree}");
```

### Notes for Future Development
- Current implementation supports only integer and float arithmetic
- String operations and identifier resolution not yet implemented
- Parser integration is the next major step
- Type system can be extended beyond current int/float support


