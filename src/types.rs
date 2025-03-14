use serde::{Deserialize, Serialize};

/// The type of types that can be assigned to ef3r terms.
#[derive(Debug, Clone, Hash, Serialize, Deserialize, PartialEq, Eq)]
pub enum ExprType {
    Any,
    Unit,
    Int,
    Float,
    String,
    Bool,
    Type,
    List(Box<ExprType>),
    Node(Box<ExprType>),
    Union(Vec<ExprType>),
    Func(Vec<ExprType>, Box<ExprType>),
    Pair(Box<ExprType>, Box<ExprType>),
}

impl ExprType {
    pub fn subtype_of(&self, other: &ExprType) -> bool {
        match (self, other) {
            (_, ExprType::Any) => true,
            (typ, ExprType::Union(types)) => types.contains(typ),
            (typ1, typ2) => typ1 == typ2,
        }
    }

    pub fn compatible_with(&self, other: &ExprType) -> bool {
        self.subtype_of(other) || other.subtype_of(self)
    }
}

impl std::fmt::Display for ExprType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprType::Any => write!(f, "Any"),
            ExprType::Unit => write!(f, "()"),
            ExprType::Int => write!(f, "Int"),
            ExprType::Float => write!(f, "Float"),
            ExprType::String => write!(f, "String"),
            ExprType::Bool => write!(f, "Bool"),
            ExprType::Type => write!(f, "Type"),
            ExprType::List(t) => write!(f, "List({})", t),
            ExprType::Node(t) => write!(f, "Node({})", t),
            ExprType::Func(args, ret) => {
                write!(f, "(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ") -> {}", ret)
            }
            ExprType::Union(types) => {
                for (i, typ) in types.iter().enumerate() {
                    if i > 0 {
                        write!(f, " | ")?;
                    }
                    write!(f, "{}", typ)?;
                }
                Ok(())
            }
            ExprType::Pair(a, b) => write!(f, "Pair({}, {})", a, b),
        }
    }
}
