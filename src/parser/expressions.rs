
use crate::parser::*;

#[derive(Clone, Debug, PartialEq)]
pub struct U32LiteralExpression {
    pub value: u32,
    pub location: SourceLocation,
}

impl Expression for U32LiteralExpression {
    fn evaluate(&self) -> ValueType {
        ValueType::U32(self.value)
    }

    fn try_evaluate_type(&self) -> Option<TypeInfo> {
        Some(TypeInfo::unsigned_32bit())
    }

    fn get_source_location(&self) -> SourceLocation {
        return self.location.clone()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct U64LiteralExpression {
    pub value: u64,
    pub location: SourceLocation,
}

impl U64LiteralExpression {
    pub fn into_expr(value: u64, location: SourceLocation) -> Box<dyn Expression> {
        Box::new(U64LiteralExpression {
            value,
            location
        })
    }
}

impl Expression for U64LiteralExpression {
    fn evaluate(&self) -> ValueType {
        ValueType::U64(self.value)
    }

    fn try_evaluate_type(&self) -> Option<TypeInfo> {
        Some(TypeInfo::unsigned_64bit())
    }

    fn get_source_location(&self) -> SourceLocation {
        return self.location.clone()
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct I32LiteralExpression {
    pub value: i32,
    pub location: SourceLocation,
}

impl Expression for I32LiteralExpression {
    fn evaluate(&self) -> ValueType {
        ValueType::I32(self.value)
    }

    fn try_evaluate_type(&self) -> Option<TypeInfo> {
        Some(TypeInfo::signed_32bit())
    }

    fn get_source_location(&self) -> SourceLocation {
        return self.location.clone()
    }
}

impl I32LiteralExpression {
    pub fn into_expr(value: i32, location: SourceLocation) -> Box<dyn Expression> {
        Box::new(I32LiteralExpression {
            value,
            location,
        })
    }
}

impl U32LiteralExpression {
    pub fn into_expr(value: u32, location: SourceLocation) -> Box<dyn Expression> {
        Box::new(U32LiteralExpression {
            value,
            location
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BoolLiteralExpression {
    pub value: bool,
    pub location: SourceLocation,
}

impl Expression for BoolLiteralExpression {
    fn evaluate(&self) -> ValueType {
        ValueType::Bool(self.value)
    }

    fn try_evaluate_type(&self) -> Option<TypeInfo> {
        Some(TypeInfo::boolean())
    }

    fn get_source_location(&self) -> SourceLocation {
        self.location.clone()
    }
}

impl BoolLiteralExpression {
    pub fn into_expr(value: bool, location: SourceLocation) -> Box<dyn Expression> {
        Box::new(BoolLiteralExpression {
            value,
            location
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct CharLiteralExpression {
    pub value: char,
    pub location: SourceLocation,
}

impl Expression for CharLiteralExpression {
    fn evaluate(&self) -> ValueType {
        ValueType::Char(self.value)
    }

    fn try_evaluate_type(&self) -> Option<TypeInfo> {
        Some(TypeInfo::character())
    }

    fn get_source_location(&self) -> SourceLocation {
        self.location.clone()
    }
}

impl CharLiteralExpression {
    pub fn into_expr(value: char, location: SourceLocation) -> Box<dyn Expression> {
        Box::new(CharLiteralExpression {
            value,
            location
        })
    }
}


#[derive(Clone, Debug, PartialEq)]
pub struct I64LiteralExpression {
    pub value: i64,
    pub location: SourceLocation,
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
            generics: None,
        })
    }

    fn get_source_location(&self) -> SourceLocation {
        self.location.clone()
    }
}

impl I64LiteralExpression {
    pub fn into_expr(value: i64, location: SourceLocation) -> Box<dyn Expression> {
        Box::new(I64LiteralExpression {
            value,
            location
        })
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StringLiteralExpression {
    pub value: String,
    pub location: SourceLocation,
}

impl Expression for StringLiteralExpression {
    fn evaluate(&self) -> ValueType {
        ValueType::String(self.value.clone())
    }

    fn try_evaluate_type(&self) -> Option<TypeInfo> {
        Some(TypeInfo::string())
    }

    fn get_source_location(&self) -> SourceLocation {
        self.location.clone()
    }
}

impl StringLiteralExpression {
    pub fn into_expr(value: String, location: SourceLocation) -> Box<dyn Expression> {
        Box::new(StringLiteralExpression {
            value,
            location
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

    pub location: SourceLocation,
}

impl Expression for VariableReferenceExpression {
    fn evaluate(&self) -> ValueType {
        ValueType::VariableReference(self.clone())
    }

    fn try_evaluate_type(&self) -> Option<TypeInfo> {
        self.assigne_type.clone()
    }

    fn get_source_location(&self) -> SourceLocation {
        self.location.clone()
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionCallExpression {
    pub name: String,
    pub arguments: Vec<Box<dyn Expression>>,

    pub location: SourceLocation,
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
            location: self.location.clone()
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

    fn get_source_location(&self) -> SourceLocation {
        self.location.clone()
    }
}

#[derive(Debug, PartialEq)]
pub struct ReturnExpression {
    pub value: Option<Box<dyn Expression>>,
    pub location: SourceLocation,
}

impl Expression for ReturnExpression {
    fn evaluate(&self) -> ValueType {
        if let Some(expr) = &self.value {
            expr.evaluate()
        }
        else {
            ValueType::Void(())
        }
    }

    fn get_source_location(&self) -> SourceLocation {
        self.location.clone()
    }

    fn try_evaluate_type(&self) -> Option<TypeInfo> {
        if let Some(expr) = &self.value {
            expr.try_evaluate_type()
        }
        else {
            None
        }
    }
}

impl Clone for ReturnExpression {
    fn clone(&self) -> Self {
        Self {
            value: match &self.value {
                Some(expr) => {
                    Some(dyn_clone::clone_box(expr.as_ref()))
                },
                None => None,
            },
            location: self.location.clone(),
        }
    }
}

impl ReturnExpression {
    pub fn into_expr(value: Option<Box<dyn Expression>>, loc: SourceLocation) -> ReturnExpression {
        ReturnExpression {
            value,
            location: loc
        }
    }
}

pub fn visit<T: Expression>(expression: &T) -> ValueType {
    expression.evaluate()
}