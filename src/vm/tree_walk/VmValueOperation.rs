use super::*;
/// Evaluates a unary operation on a VmValue.
///
/// This is a standalone function that handles negation and logical NOT operations 
/// on numeric and boolean values. It doesn't require any VM state and can be used independently.
///
/// # Arguments
///
/// * `operator` - The unary operator
/// * `operand` - The operand value
///
/// # Returns
///
/// A `VmResult` containing the computed result or an error.
pub fn evaluate_unary_op(operator: &UnaryOperator, operand: &VmValue) -> VmResult {
    match (operator, operand) {
        (UnaryOperator::Minus, VmValue::Literal(Literal::Integer(i))) => Ok(Literal::Integer(-i).into()),
        (UnaryOperator::Minus, VmValue::Literal(Literal::Float(f))) => Ok(Literal::Float(-f).into()),
        (UnaryOperator::Not, VmValue::Literal(Literal::Bool(b))) => Ok(Literal::Bool(!b).into()),
        (_, VmValue::Product(_)) => Err(VmError::InvalidOperation("Product operations not yet implemented".to_string())),
        _ => Err(VmError::InvalidOperation(format!(
            "Cannot apply {operator:?} to {operand:?}",
        ))),
    }
}

/// Evaluates a binary operation between two VmValues.
///
/// This is a standalone function that handles arithmetic, comparison, and logical 
/// operations with type coercion between integers and floats when necessary.
/// It doesn't require any VM state and can be used independently.
///
/// # Arguments
///
/// * `left` - The left operand value
/// * `operator` - The binary operator
/// * `right` - The right operand value
///
/// # Returns
///
/// A `VmResult` containing the computed result or an error.
pub fn evaluate_binary_op(
    left: &VmValue,
    operator: &BinaryOperator,
    right: &VmValue,
) -> VmResult {
    match (left, right) {
        // Bool operations
        (VmValue::Literal(Literal::Bool(l)), VmValue::Literal(Literal::Bool(r))) => match operator {
            BinaryOperator::Equal => Ok(Literal::Bool(l == r).into()),
            BinaryOperator::NotEqual => Ok(Literal::Bool(l != r).into()),
            BinaryOperator::And => Ok(Literal::Bool(*l && *r).into()),
            BinaryOperator::Or => Ok(Literal::Bool(*l || *r).into()),
            BinaryOperator::Less => Ok(Literal::Bool(!*l && *r).into()), // false < true
            BinaryOperator::LessEqual => Ok(Literal::Bool(!*l || *r).into()), // false <= true, true <= true
            BinaryOperator::Greater => Ok(Literal::Bool(*l && !*r).into()),   // true > false
            BinaryOperator::GreaterEqual => Ok(Literal::Bool(*l || !*r).into()), // true >= false, true >= true
            _ => Err(VmError::InvalidOperation(format!(
                "Cannot apply {operator:?} to Bool"
            ))),
        },
        // Integer operations
        (VmValue::Literal(Literal::Integer(l)), VmValue::Literal(Literal::Integer(r))) => match operator {
            BinaryOperator::Plus => Ok(Literal::Integer(l + r).into()),
            BinaryOperator::Minus => Ok(Literal::Integer(l - r).into()),
            BinaryOperator::Multiply => Ok(Literal::Integer(l * r).into()),
            BinaryOperator::Divide => {
                if *r == 0 {
                    Err(VmError::DivisionByZero)
                } else {
                    Ok(Literal::Integer(l / r).into())
                }
            }
            BinaryOperator::Modulo => {
                if *r == 0 {
                    Err(VmError::DivisionByZero)
                } else {
                    Ok(Literal::Integer(l % r).into())
                }
            }
            BinaryOperator::Equal => Ok(Literal::Bool(l == r).into()),
            BinaryOperator::NotEqual => Ok(Literal::Bool(l != r).into()),
            BinaryOperator::Less => Ok(Literal::Bool(l < r).into()),
            BinaryOperator::LessEqual => Ok(Literal::Bool(l <= r).into()),
            BinaryOperator::Greater => Ok(Literal::Bool(l > r).into()),
            BinaryOperator::GreaterEqual => Ok(Literal::Bool(l >= r).into()),
            _ => Err(VmError::InvalidOperation(format!(
                "Cannot apply {operator:?} to integer"
            ))),
        },
        // Float operations
        (VmValue::Literal(Literal::Float(l)), VmValue::Literal(Literal::Float(r))) => match operator {
            BinaryOperator::Plus => Ok(Literal::Float(l + r).into()),
            BinaryOperator::Minus => Ok(Literal::Float(l - r).into()),
            BinaryOperator::Multiply => Ok(Literal::Float(l * r).into()),
            BinaryOperator::Divide => {
                if *r == 0.0 {
                    Err(VmError::DivisionByZero)
                } else {
                    Ok(Literal::Float(l / r).into())
                }
            }
            BinaryOperator::Modulo => {
                if *r == 0.0 {
                    Err(VmError::DivisionByZero)
                } else {
                    Ok(Literal::Float(l % r).into())
                }
            }
            BinaryOperator::Equal => Ok(Literal::Bool((l - r).abs() < f64::EPSILON).into()),
            BinaryOperator::NotEqual => Ok(Literal::Bool((l - r).abs() >= f64::EPSILON).into()),
            BinaryOperator::Less => Ok(Literal::Bool(l < r).into()),
            BinaryOperator::LessEqual => Ok(Literal::Bool(l <= r).into()),
            BinaryOperator::Greater => Ok(Literal::Bool(l > r).into()),
            BinaryOperator::GreaterEqual => Ok(Literal::Bool(l >= r).into()),
            _ => Err(VmError::InvalidOperation(format!(
                "Cannot apply {operator:?} to float"
            ))),
        },

        // Handle Product types - all operations return error for now
        (VmValue::Product(_), _) | (_, VmValue::Product(_)) => {
            Err(VmError::InvalidOperation("Product operations not yet implemented".to_string()))
        }
        
        // Type mismatch for other combinations
        _ => Err(VmError::TypeMismatch),
    }
}
