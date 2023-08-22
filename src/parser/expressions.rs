
use crate::parser::*;

#[derive(Clone, Debug, PartialEq)]
pub struct U32LiteralExpression {
    pub value: u32,
}


impl Expression for U32LiteralExpression {
    fn evaluate(&self) -> ValueType {
        ValueType::U32(self.value)
    }

    fn try_evaluate_type(&self) -> Option<TypeInfo> {
        Some(TypeInfo {
            name: "u32".to_owned(),
            size: 4,
            needs_to_resolve_size: false,
            has_been_resolved: true,
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct I32LiteralExpression {
    pub value: i32,
}

impl Expression for I32LiteralExpression {
    fn evaluate(&self) -> ValueType {
        ValueType::I32(self.value)
    }

    fn try_evaluate_type(&self) -> Option<TypeInfo> {
        Some(TypeInfo {
            name: "i32".to_owned(),
            size: 4,
            needs_to_resolve_size: false,
            has_been_resolved: true,
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct U64LiteralExpression {
    pub value: u64,
}

impl Expression for U64LiteralExpression {
    fn evaluate(&self) -> ValueType {
        ValueType::U64(self.value)
    }

    fn try_evaluate_type(&self) -> Option<TypeInfo> {
        Some(TypeInfo {
            name: "u64".to_owned(),
            size: 8,
            needs_to_resolve_size: false,
            has_been_resolved: true,
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct I64LiteralExpression {
    pub value: i64,
}

impl Expression for I64LiteralExpression {
    fn evaluate(&self) -> ValueType {
        ValueType::I64(self.value)
    }

    fn try_evaluate_type(&self) -> Option<TypeInfo> {
        Some(TypeInfo {
            name: "i64".to_owned(),
            size: 8,
            needs_to_resolve_size: false,
            has_been_resolved: true,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableReferenceExpression {
    // The name of the variable, this is for the interpreter to resolve.
    // The parser should not care about what value this actually holds.
    pub name: String,
    // The type of the variable being assigned to, if applicable.
    pub assigne_type: Option<TypeInfo>,
}

impl Expression for VariableReferenceExpression {
    fn evaluate(&self) -> ValueType {
        ValueType::VariableReference(self.clone())
    }

    fn try_evaluate_type(&self) -> Option<TypeInfo> {
        self.assigne_type.clone()
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionCallExpression {
    pub name: String,
    pub arguments: Vec<Box<dyn Expression>>,
}

impl Clone for FunctionCallExpression {
    fn clone(&self) -> Self {
        let mut new_vector: Vec<Box<dyn Expression>> = Vec::new();
        for argument in &self.arguments {
            new_vector.push(dyn_clone::clone_box(&**argument));
        }
        FunctionCallExpression {
            name: self.name.clone(),
            arguments: new_vector,
        }
    }
}

impl Expression for FunctionCallExpression {
    fn evaluate(&self) -> ValueType {
        ValueType::FunctionCall(self.clone())
    }

    fn try_evaluate_type(&self) -> Option<TypeInfo> {
        None
    }
}

pub fn visit<T: Expression>(expression: &T) -> ValueType {
    expression.evaluate()
}