use std::{fmt::{Debug, Display}, mem::size_of};
use once_cell::sync::Lazy;
use std::{sync::Mutex, collections::HashMap};

use super::expressions::{VariableReferenceExpression, FunctionCallExpression};

trait VecExt<T> {
    fn internal_copy(&self) -> Vec<T>;
}

impl<T: Clone> VecExt<T> for Vec<T> {
    fn internal_copy(&self) -> Vec<T> {
        self.to_vec()
    }
}

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
    pub fn serialize(&self) -> String {
        return match &self.name {
            name if name == "u32" => ".32u".to_owned(),
            name if name == "i32" => ".32".to_owned(),
            name if name == "u64" => ".64u".to_owned(),
            name if name == "i64" => ".64".to_owned(),
            name if name == "f32" => ".f4".to_owned(),
            name if name == "f64" => ".f8".to_owned(),
            name if name == "bool" => ".1".to_owned(),
            name if name == "char" => ".1c".to_owned(),
            name if name == "string" => ".S".to_owned(),
            name if name == "void" => ".V".to_owned(),
            name if name == "size_t" => ".st_86".to_owned(),
            _ => self.name.clone(),
        }
    }

    pub fn void() -> TypeInfo {
        TypeInfo {
            name: "void".to_owned(),
            size: 0,
            needs_to_resolve_size: false,
            has_been_resolved: true,
        }
    }

    pub fn unsigned_32bit() -> TypeInfo {
        TypeInfo {
            name: "u32".to_owned(),
            size: 4,
            needs_to_resolve_size: false,
            has_been_resolved: true,
        }
    } 

    pub fn signed_32bit() -> TypeInfo {
        TypeInfo {
            name: "i32".to_owned(),
            size: 4,
            needs_to_resolve_size: false,
            has_been_resolved: true,
        }
    }

    pub fn unsigned_64bit() -> TypeInfo {
        TypeInfo {
            name: "u64".to_owned(),
            size: 8,
            needs_to_resolve_size: false,
            has_been_resolved: true,
        }
    }

    pub fn signed_64bit() -> TypeInfo {
        TypeInfo {
            name: "i64".to_owned(),
            size: 8,
            needs_to_resolve_size: false,
            has_been_resolved: true,
        }
    }

    pub fn float_32bit() -> TypeInfo {
        TypeInfo {
            name: "f32".to_owned(),
            size: 4,
            needs_to_resolve_size: false,
            has_been_resolved: true,
        }
    }

    pub fn float_64bit() -> TypeInfo {
        TypeInfo {
            name: "f64".to_owned(),
            size: 8,
            needs_to_resolve_size: false,
            has_been_resolved: true,
        }
    }

    pub fn boolean() -> TypeInfo {
        TypeInfo {
            name: "bool".to_owned(),
            size: 1,
            needs_to_resolve_size: false,
            has_been_resolved: true,
        }
    }

    pub fn character() -> TypeInfo {
        TypeInfo {
            name: "char".to_owned(),
            size: 1,
            needs_to_resolve_size: false,
            has_been_resolved: true,
        }
    }

    pub fn string() -> TypeInfo {
        TypeInfo {
            name: "string".to_owned(),
            // string is a pointer type.
            size: size_of::<usize>(),
            needs_to_resolve_size: true,
            has_been_resolved: true,
        }
    }

    pub fn size_type() -> TypeInfo {
        TypeInfo {
            name: "size_t".to_owned(),
            size: size_of::<usize>(),
            needs_to_resolve_size: false,
            has_been_resolved: true,
        }
    }

    pub fn is_builtin_integral(&self) -> bool {
        return match &self.name[..] {
            "u32" | "i32" | "u64" | "i64" | "bool" | "char" => true,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct Signature {
    pub name: String,
    pub return_type: TypeInfo,
    pub arguments: Vec<TypeInfo>,
}

impl Clone for Signature {
    fn clone(&self) -> Self {
        Signature {
            name: self.name.clone(),
            return_type: self.return_type.clone(),
            arguments: self.arguments.internal_copy(),
        }
    }
}

impl Signature {
    pub fn new(name: &String, return_type: &TypeInfo, arguments: Vec<TypeInfo>) -> Signature {
        Signature {
            name: name.clone(),
            return_type: return_type.clone(),
            arguments: arguments.internal_copy(),
        }
    }

    pub fn serialize(&self) -> String {
        let mut result = String::new();

        result.push_str(&self.name);
        result.push_str("@");
        result.push_str(&self.return_type.serialize());

        for item in self.arguments.iter() {
            result.push_str("@");
            result.push_str(&item.serialize());
        }

        result
    }
}

impl PartialEq for Signature {
    fn eq(&self, other: &Self) -> bool {
        return self.serialize() == other.serialize();
    }
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

impl Clone for StructField {
    fn clone(&self) -> Self {
        StructField {
            name: self.name.clone(),
            type_info: self.type_info.clone(),
            value: match &self.value {
                ValueType::U32(value) => ValueType::U32(value.clone()),
                ValueType::I32(value) => ValueType::I32(value.clone()),
                ValueType::U64(value) => ValueType::U64(value.clone()),
                ValueType::I64(value) => ValueType::I64(value.clone()),
                ValueType::F32(value) => ValueType::F32(value.clone()),
                ValueType::F64(value) => ValueType::F64(value.clone()),
                ValueType::Bool(value) => ValueType::Bool(value.clone()),
                ValueType::String(value) => ValueType::String(value.clone()),
                ValueType::Char(value) => ValueType::Char(value.clone()),
                ValueType::Void(_) => ValueType::Void(()),
                ValueType::Struct(value) => ValueType::Struct(value.clone()),
                ValueType::Enum(value) => ValueType::Enum(value.clone()),
                ValueType::VariableReference(value) => ValueType::VariableReference(value.clone()),
                ValueType::FunctionCall(value) => ValueType::FunctionCall(value.clone()),
            },
        }
    }
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

impl Clone for ParameterFacts {
    fn clone(&self) -> Self {
        ParameterFacts {
            name: self.name.clone(),
            type_info: self.type_info.clone(),
            initializer: match &self.initializer {
                Some(expr) => Some(dyn_clone::clone_box(&**expr)),
                None => None,
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionExpressionFacts {
    pub name: String,
    pub args: Option<Vec<ParameterFacts>>
}

impl Clone for FunctionExpressionFacts {
    fn clone(&self) -> Self {
        let new_args = match &self.args {
            Some(args) => Some(args.internal_copy()),
            None => None,
        };

        FunctionExpressionFacts {
            name: self.name.clone(),
            args: new_args,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct StructFacts {
    pub name: String,
    pub fields: Option<Vec<StructField>>
}

impl Clone for StructFacts {
    fn clone(&self) -> Self {
        let new_fields = match &self.fields {
            Some(fields) => Some(fields.internal_copy()),
            None => None,
        };

        StructFacts {
            name: self.name.clone(),
            fields: new_fields,
        }
    }
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

impl Clone for EnumFacts {
    fn clone(&self) -> Self {
        let mut new_variants: Vec<EnumVariantFacts> = Vec::new();
        for variant in &self.variants {
            new_variants.push(EnumVariantFacts {
                name: variant.name.clone(),
                values: match &variant.values {
                    Some(values) => Some(values.internal_copy()),
                    None => None,
                }
            });
        }

        EnumFacts {
            name: self.name.clone(),
            variants: new_variants,
        }
    }
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
            ValueType::FunctionCall(expr) => {
                write!(f, "<call to '{}'>", expr.name)
            },
        }
    }
}

use dyn_clone::DynClone;

#[derive(Debug, PartialEq)]
pub struct Block {
    // All statements in the block to be executed.
    pub statements: Vec<AstNode>,
    // The function that owns this block. This is to be used for 
    // resolving variables.
    pub owner: Option<Signature>,
}

impl Clone for Block {
    fn clone(&self) -> Self {
        let new_statements = self.statements.internal_copy();
        let new_owner = match &self.owner {
            Some(owner) => Some(owner.clone()),
            None => None,
        };

        Self {
            statements: new_statements,
            owner: new_owner,
        }
    }
}

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
        f.write_str(&format!("{}", self.evaluate()))
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionDeclarationFacts {
    pub name: String,
    pub return_type: TypeInfo,
    pub arguments: Vec<ParameterFacts>,
    pub body: Block,
}

impl Clone for FunctionDeclarationFacts {
    fn clone(&self) -> Self {
        let new_vec = self.arguments.internal_copy();

        return Self {
            name: self.name.clone(),
            return_type: self.return_type.clone(),
            arguments: new_vec,
            body: self.body.clone(),
        }
    }
}

#[derive(Debug)]
pub struct AssignmentFacts {
    pub constant: bool,
    pub name: String,
    pub type_info: Option<TypeInfo>,
    pub expression: Box<dyn Expression>
}

impl Clone for AssignmentFacts {
    fn clone(&self) -> Self {
        return AssignmentFacts {
            constant: self.constant,
            name: self.name.clone(),
            type_info: match &self.type_info {
                Some(type_info) => Some(type_info.clone()),
                None => None,
            },
            expression: dyn_clone::clone_box(&*self.expression),
        }
    }
}

impl PartialEq for AssignmentFacts {
    fn eq(&self, other: &Self) -> bool {
        return self.name == other.name;
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeAliasFacts {
    pub name: String,
    pub type_info: TypeInfo,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UseStatementFacts {
    // each string represents a part of the path.
    // so, "std::io::println" would be represented as:
    // ["std", "io", "println"]
    // This way, it can be walked recursively.
    pub path: Vec<String>,
}


#[derive(Debug)]
pub enum AstNode {
    Assignment(AssignmentFacts),
    Expression(Box<dyn Expression>),
    Struct(StructFacts),
    Enum(EnumFacts),
    FunctionCall(FunctionExpressionFacts),
    FunctionDeclaration(FunctionDeclarationFacts),
    Body(Block),
    TypeAlias(TypeAliasFacts),
    UseStatement(UseStatementFacts),

    // in the case of `;`, we don't need to store anything.
    EmptyExpr,
}

impl Clone for AstNode {
    fn clone(&self) -> Self {
        match self {
            AstNode::Assignment(facts) => {
                return AstNode::Assignment(facts.clone());
            },
            AstNode::Expression(expr) => {
                return AstNode::Expression(dyn_clone::clone_box(&**expr));
            },
            AstNode::Struct(facts) => {
                return AstNode::Struct(facts.clone());
            },
            AstNode::Enum(facts) => {
                return AstNode::Enum(facts.clone());
            },
            AstNode::FunctionCall(facts) => {
                return AstNode::FunctionCall(facts.clone());
            },
            AstNode::EmptyExpr => {
                return AstNode::EmptyExpr;
            },
            AstNode::FunctionDeclaration(facts) => {
                return AstNode::FunctionDeclaration(facts.clone());
            },
            AstNode::Body(block) => {
                return AstNode::Body(block.clone());
            },
            AstNode::TypeAlias(alias) => {
                return AstNode::TypeAlias(alias.clone());
            },
            AstNode::UseStatement(statement) => {
                return AstNode::UseStatement(statement.clone());
            }
        }
    }
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
            AstNode::FunctionCall(facts) => {
                match other {
                    AstNode::FunctionCall(other_facts) => {
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
            AstNode::FunctionDeclaration(facts) => {
                match other {
                    AstNode::FunctionDeclaration(other_facts) => {
                        return *facts == *other_facts;
                    },
                    _ => return false,
                }
            },
            AstNode::Body(block) => {
                match other {
                    AstNode::Body(other_block) => {
                        return block == other_block;
                    },
                    _ => return false,
                }
            },
            AstNode::TypeAlias(alias) => {
                match other {
                    AstNode::TypeAlias(other_alias) => {
                        return alias == other_alias;
                    },
                    _ => return false,
                }
            },
            AstNode::UseStatement(statement) => {
                match other {
                    AstNode::UseStatement(other_statement) => {
                        return statement == other_statement;
                    },
                    _ => return false,
                }
            }
        }
    }
}