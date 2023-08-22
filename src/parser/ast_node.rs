use std::{fmt::{Debug, Display}, mem::size_of};
use once_cell::sync::Lazy;
use std::{sync::Mutex, collections::HashMap};

use super::expressions::{VariableReferenceExpression, FunctionCallExpression};

#[derive(Debug, PartialEq, Clone)]
pub struct TypeInfo {
    pub name: String,
    pub size: usize,
    pub needs_to_resolve_size: bool,

    /* This field describes if the type has any information about it yet. */
    /* This does not tell you that the type exists, only whether we have info about it yet. */
    pub has_been_resolved: bool,
}

impl TypeInfo {
    pub fn unresolved() -> TypeInfo {
        TypeInfo {
            name: String::new(),
            size: 0,
            needs_to_resolve_size: true,
            has_been_resolved: false,
        }
    }
}

static BUILTIN_TYPES: Lazy<Mutex<HashMap<&'static str, TypeInfo>>> = Lazy::new(|| {
    let mut map = HashMap::new();

    map.insert("u32", TypeInfo {
        name: "u32".to_owned(),
        size: 4,
        needs_to_resolve_size: false,
        has_been_resolved: true,
    });

    map.insert("i32", TypeInfo {
        name: "i32".to_owned(),
        size: 4,
        needs_to_resolve_size: false,
        has_been_resolved: true,
    });

    map.insert("u64", TypeInfo {
        name: "u64".to_owned(),
        size: 8,
        needs_to_resolve_size: false,
        has_been_resolved: true,
    });

    map.insert("i64", TypeInfo {
        name: "i64".to_owned(),
        size: 8,
        needs_to_resolve_size: false,
        has_been_resolved: true,
    });

    map.insert("f32", TypeInfo {
        name: "f32".to_owned(),
        size: 4,
        needs_to_resolve_size: false,
        has_been_resolved: true,
    });

    map.insert("f64", TypeInfo {
        name: "f64".to_owned(),
        size: 8,
        needs_to_resolve_size: false,
        has_been_resolved: true,
    });

    map.insert("bool", TypeInfo {
        name: "bool".to_owned(),
        size: 1,
        needs_to_resolve_size: false,
        has_been_resolved: true,
    });

    map.insert("char", TypeInfo {
        name: "char".to_owned(),
        size: 1,
        needs_to_resolve_size: false,
        has_been_resolved: true,
    });

    map.insert("string", TypeInfo {
        name: "string".to_owned(),
        size: size_of::<usize>(),
        needs_to_resolve_size: true,
        has_been_resolved: true,
    });

    map.insert("void", TypeInfo {
        name: "void".to_owned(),
        size: 0,
        needs_to_resolve_size: false,
        has_been_resolved: true,
    });

    map.insert("size_t", TypeInfo {
        name: "size_t".to_owned(),
        size: size_of::<usize>(),
        needs_to_resolve_size: false,
        has_been_resolved: true,
    });

    Mutex::new(map)
});

impl TypeInfo {
    pub fn size_was_resolved(&self) -> bool {
        return !self.needs_to_resolve_size;
    } 

    pub fn try_resolve_size(&mut self) {
        // see if BUILTIN_TYPES has the type
        let builtin_types = BUILTIN_TYPES.lock().unwrap();

        let type_info = match builtin_types.get(self.name.as_str()) {
            Some(type_info) => type_info,
            None => return,
        };

        self.name = type_info.name.clone();
        self.size = type_info.size;
        self.needs_to_resolve_size = false;
    }
}

#[derive(Debug, PartialEq)]
pub struct StructField {
    pub name: String,
    pub type_info: TypeInfo,
    pub value: ValueType
}

impl StructField {
    pub fn type_is(&self, name: &str) -> bool {
        return self.type_info.name == name;
    }

    pub fn value_is(&self, value: &ValueType) -> bool {
        return self.value == *value;
    }
}

#[derive(Debug, PartialEq)]
pub struct ParameterFacts {
    pub name: String,
    pub type_info: TypeInfo,
    pub initializer: Option<Box<dyn Expression>>
}

#[derive(Debug, PartialEq)]
pub struct FunctionFacts {
    pub name: String,
    pub args: Option<Vec<ParameterFacts>>,
    pub body: Vec<AstNode>
}

#[derive(Debug, PartialEq)]
pub struct StructFacts {
    pub name: String,
    pub fields: Option<Vec<StructField>>
}

#[derive(Debug, PartialEq)]
pub struct EnumVariantFacts {
    pub name: String,
    pub values: Option<Vec<TypeInfo>>
}

#[derive(Debug, PartialEq)]
pub struct EnumFacts {
    pub name: String,
    pub variants: Vec<EnumVariantFacts>
}

#[derive(Debug)]
pub enum ValueType {
    U32(u32), 
    I32(i32), 
    U64(u64), 
    I64(i64), 
    F32(f32), 
    F64(f64), 
    Bool(bool), 
    String(String), 
    Char(char), 
    Void(()), 
    Struct(StructFacts),
    Enum(EnumFacts),

    VariableReference(VariableReferenceExpression),
    FunctionCall(FunctionCallExpression)
}

impl PartialEq for ValueType {
    fn eq(&self, other: &Self) -> bool {
        match self {
            ValueType::U32(value) => {
                match other {
                    ValueType::U32(other_value) => {
                        return value == other_value;
                    },
                    _ => return false,
                }
            },
            ValueType::I32(value) => {
                match other {
                    ValueType::I32(other_value) => {
                        return value == other_value;
                    },
                    _ => return false,
                }
            },
            ValueType::U64(value) => {
                match other {
                    ValueType::U64(other_value) => {
                        return value == other_value;
                    },
                    _ => return false,
                }
            },
            ValueType::I64(value) => {
                match other {
                    ValueType::I64(other_value) => {
                        return value == other_value;
                    },
                    _ => return false,
                }
            },
            ValueType::F32(value) => {
                match other {
                    ValueType::F32(other_value) => {
                        return value == other_value;
                    },
                    _ => return false,
                }
            },
            ValueType::F64(value) => {
                match other {
                    ValueType::F64(other_value) => {
                        return value == other_value;
                    },
                    _ => return false,
                }
            },
            ValueType::Bool(value) => {
                match other {
                    ValueType::Bool(other_value) => {
                        return value == other_value;
                    },
                    _ => return false,
                }
            },
            ValueType::String(value) => {
                match other {
                    ValueType::String(other_value) => {
                        return value == other_value;
                    },
                    _ => return false,
                }
            },
            ValueType::Char(value) => {
                match other {
                    ValueType::Char(other_value) => {
                        return value == other_value;
                    },
                    _ => return false,
                }
            },
            ValueType::Void(_) => {
                match other {
                    ValueType::Void(_) => {
                        return true;
                    },
                    _ => return false,
                }
            },
            ValueType::Struct(value) => {
                match other {
                    ValueType::Struct(other_value) => {
                        return value == other_value;
                    },
                    _ => return false,
                }
            },
            ValueType::Enum(value) => {
                match other {
                    ValueType::Enum(other_value) => {
                        return value == other_value;
                    },
                    _ => return false
                }
            },
            ValueType::VariableReference(value) => {
                match other {
                    ValueType::VariableReference(other_value) => {
                        return value == other_value;
                    },
                    _ => return false,
                }
            },
            ValueType::FunctionCall(call) => {
                match other {
                    ValueType::FunctionCall(other_call) => {
                        return call == other_call;
                    },
                    _ => return false,
                }
            }
        }
    }
}

impl Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueType::U32(value) => write!(f, "{}", value),
            ValueType::I32(value) => write!(f, "{}", value),
            ValueType::U64(value) => write!(f, "{}", value),
            ValueType::I64(value) => write!(f, "{}", value),
            ValueType::F32(value) => write!(f, "{}", value),
            ValueType::F64(value) => write!(f, "{}", value),
            ValueType::Bool(value) => write!(f, "{}", value),
            ValueType::String(value) => write!(f, "{}", value),
            ValueType::Char(value) => write!(f, "{}", value),
            ValueType::Void(_) => write!(f, "(void)"),
            ValueType::Struct(value) => write!(f, "{:?}", value),
            ValueType::Enum(value) => write!(f, "{:?}", value),
            ValueType::VariableReference(value) => write!(f, "&{:?}", value),
            ValueType::FunctionCall(expr) => write!(f, "{}({} args...)", expr.name, expr.arguments.len()),
        }
    }
}

use dyn_clone::DynClone;

pub trait Expression: DynClone {
    fn evaluate(&self) -> ValueType;

    fn try_evaluate_type(&self) -> Option<TypeInfo> {
        return None;
    }
}

impl PartialEq for dyn Expression {
    fn eq(&self, other: &Self) -> bool {
        return self.evaluate() == other.evaluate();
    }
}

impl Debug for dyn Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("Expression: {}", self.evaluate()))
    }
}

#[derive(Debug)]
pub struct AssignmentFacts {
    pub is_const: bool,
    pub name: String,
    pub type_info: Option<TypeInfo>,
    pub value: Box<dyn Expression>
}

impl PartialEq for AssignmentFacts {
    fn eq(&self, other: &Self) -> bool {
        return self.name == other.name;
    }
}


#[derive(Debug)]
pub enum AstNode {
    Assignment(AssignmentFacts),
    Expression(Box<dyn Expression>),
    Struct(StructFacts),
    Enum(EnumFacts),
    Function(FunctionFacts),

    // in the case of `;`, we don't need to store anything.
    EmptyExpr,
}

impl PartialEq for AstNode {
    fn eq(&self, other: &Self) -> bool {
        match self {
            AstNode::Assignment(facts) => {
                match other {
                    AstNode::Assignment(other_facts) => {
                        return facts == other_facts;
                    },
                    _ => return false,
                }
            },
            AstNode::Expression(expr) => {
                match other {
                    AstNode::Expression(other_expr) => {
                        return expr == other_expr;
                    },
                    _ => return false,
                }
            },
            AstNode::Struct(facts) => {
                match other {
                    AstNode::Struct(other_facts) => {
                        return facts == other_facts;
                    },
                    _ => return false,
                }
            },
            AstNode::Enum(facts) => {
                match other {
                    AstNode::Enum(other_facts) => {
                        return facts == other_facts;
                    },
                    _ => return false,
                }
            },
            AstNode::Function(facts) => {
                match other {
                    AstNode::Function(other_facts) => {
                        return facts == other_facts;
                    },
                    _ => return false,
                }
            },
            AstNode::EmptyExpr => {
                match other {
                    AstNode::EmptyExpr => {
                        return true;
                    },
                    _ => return false,
                }
            },
        }
    }
}