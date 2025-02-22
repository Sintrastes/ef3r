use crate::{
    ast::traced_expr::{TracedExpr, TracedExprRec},
    interpreter::VariableId,
    types::ExprType,
};

/// Trait to map Rust types to ExprTypes
pub trait ExprTypeable {
    fn expr_type() -> ExprType;
    fn try_from_expr(expr: &TracedExprRec<VariableId>) -> Option<Self>
    where
        Self: Sized;
    fn to_expr(self) -> TracedExprRec<VariableId>;
}

// Implementations for basic types
impl ExprTypeable for i32 {
    fn expr_type() -> ExprType {
        ExprType::Int
    }
    fn try_from_expr(expr: &TracedExprRec<VariableId>) -> Option<Self> {
        if let TracedExprRec::Int(x) = expr {
            Some(*x)
        } else {
            None
        }
    }
    fn to_expr(self) -> TracedExprRec<VariableId> {
        TracedExprRec::Int(self)
    }
}

impl ExprTypeable for f32 {
    fn expr_type() -> ExprType {
        ExprType::Float
    }
    fn try_from_expr(expr: &TracedExprRec<VariableId>) -> Option<Self> {
        if let TracedExprRec::Float(x) = expr {
            Some(*x)
        } else {
            None
        }
    }
    fn to_expr(self) -> TracedExprRec<VariableId> {
        TracedExprRec::Float(self)
    }
}

impl ExprTypeable for String {
    fn expr_type() -> ExprType {
        ExprType::String
    }
    fn try_from_expr(expr: &TracedExprRec<VariableId>) -> Option<Self> {
        if let TracedExprRec::String(x) = expr {
            Some(x.clone())
        } else {
            None
        }
    }
    fn to_expr(self) -> TracedExprRec<VariableId> {
        TracedExprRec::String(self)
    }
}

impl ExprTypeable for bool {
    fn expr_type() -> ExprType {
        ExprType::Bool
    }
    fn try_from_expr(expr: &TracedExprRec<VariableId>) -> Option<Self> {
        if let TracedExprRec::Bool(x) = expr {
            Some(*x)
        } else {
            None
        }
    }
    fn to_expr(self) -> TracedExprRec<VariableId> {
        TracedExprRec::Bool(self)
    }
}

impl ExprTypeable for () {
    fn expr_type() -> ExprType {
        ExprType::Unit
    }
    fn try_from_expr(expr: &TracedExprRec<VariableId>) -> Option<Self> {
        if let TracedExprRec::Unit = expr {
            Some(())
        } else {
            None
        }
    }
    fn to_expr(self) -> TracedExprRec<VariableId> {
        TracedExprRec::Unit
    }
}

impl ExprTypeable for Vec<TracedExpr<VariableId>> {
    fn expr_type() -> ExprType {
        ExprType::List(Box::new(ExprType::Any))
    }
    fn try_from_expr(expr: &TracedExprRec<VariableId>) -> Option<Self> {
        if let TracedExprRec::List(x) = expr {
            Some(x.clone())
        } else {
            None
        }
    }
    fn to_expr(self) -> TracedExprRec<VariableId> {
        TracedExprRec::List(self)
    }
}

impl ExprTypeable for TracedExprRec<VariableId> {
    fn expr_type() -> ExprType {
        ExprType::Any
    }
    fn try_from_expr(expr: &TracedExprRec<VariableId>) -> Option<Self> {
        Some(expr.clone())
    }
    fn to_expr(self) -> TracedExprRec<VariableId> {
        self
    }
}

macro_rules! build_function {
    // Pattern for single argument function with type checking
    ($debugger:ty, $name:expr, $res_type:expr, |$ctx:ident, $param:ident: $type:ty| $body:expr) => {
        FunctionDefinition::<$debugger> {
            argument_types: vec![<$type>::expr_type()],
            result_type: $res_type,
            name: $name.to_string(),
            definition: |$ctx, xs: &[TracedExpr<VariableId>]| {
                let expr = xs.get(0).ok_or(
                    EvaluationError::WrongNumberOfArguments {
                        expected: 1,
                        actual: 0,
                        for_function: $name.to_string(),
                    },
                )?;

                let $param = <$type>::try_from_expr(&expr.evaluated).ok_or(
                    EvaluationError::TypeError {
                        expected: <$type>::expr_type(),
                        actual: type_of(
                            &$ctx.expression_context.read(),
                            &expr.evaluated,
                        )
                        .unwrap_or(ExprType::Any),
                        at_loc: $name.to_string(),
                    },
                )?;

                $body.map(|x| x.to_expr())
            },
        }
    };

    // Pattern for two argument function with type checking
    ($debugger:ty, $name:expr, $res_type:expr, |$ctx:ident, $param1:ident: $type1:ty, $param2:ident: $type2:ty| $body:expr) => {
        FunctionDefinition::<$debugger> {
            name: $name.to_string(),
            argument_types: vec![<$type1>::expr_type(), <$type2>::expr_type()],
            result_type: $res_type,
            definition: |$ctx, xs: &[TracedExpr<VariableId>]| {
                let expr1 = xs.get(0).ok_or(
                    EvaluationError::WrongNumberOfArguments {
                        expected: 2,
                        actual: 0,
                        for_function: $name.to_string(),
                    },
                )?;

                let expr2 = xs.get(1).ok_or(
                    EvaluationError::WrongNumberOfArguments {
                        expected: 2,
                        actual: 1,
                        for_function: $name.to_string(),
                    },
                )?;

                let $param1 = <$type1>::try_from_expr(&expr1.evaluated).ok_or(
                    EvaluationError::TypeError {
                        expected: <$type1>::expr_type(),
                        actual: type_of(
                            &$ctx.expression_context.read(),
                            &expr1.evaluated,
                        )
                        .unwrap_or(ExprType::Any),
                        at_loc: $name.to_string(),
                    },
                )?;

                let $param2 = <$type2>::try_from_expr(&expr2.evaluated).ok_or(
                    EvaluationError::TypeError {
                        expected: <$type2>::expr_type(),
                        actual: type_of(
                            &$ctx.expression_context.read(),
                            &expr2.evaluated,
                        )
                        .unwrap_or(ExprType::Any),
                        at_loc: $name.to_string(),
                    },
                )?;

                $body.map(|x| x.to_expr())
            },
        }
    };

    // Pattern for single argument function without type checking
    ($debugger:ty, $name:expr, $res_type:expr, $arg_types:expr, |$ctx:ident, $param:ident| $body:expr) => {
        FunctionDefinition::<$debugger> {
            name: $name.to_string(),
            argument_types: $arg_types,
            result_type: $res_type,
            definition: |$ctx, xs: &[TracedExpr<VariableId>]| {
                let $param = xs
                    .get(0)
                    .ok_or(EvaluationError::WrongNumberOfArguments {
                        expected: 1,
                        actual: 0,
                        for_function: $name.to_string(),
                    })?
                    .clone();

                $body
            },
        }
    };

    // Pattern for two argument function without type checking.
    ($debugger:ty, $name:expr, $res_type:expr, $arg_types:expr, |$ctx:ident, $param1:ident, $param2:ident| $body:expr) => {
        FunctionDefinition::<$debugger> {
            name: $name.to_string(),
            argument_types: $arg_types,
            result_type: $res_type,
            definition: |$ctx, xs: &[TracedExpr<VariableId>]| {
                let $param1 = xs
                    .get(0)
                    .ok_or(EvaluationError::WrongNumberOfArguments {
                        expected: 2,
                        actual: 0,
                        for_function: $name.to_string(),
                    })?
                    .clone();

                let $param2 = xs
                    .get(1)
                    .ok_or(EvaluationError::WrongNumberOfArguments {
                        expected: 2,
                        actual: 1,
                        for_function: $name.to_string(),
                    })?
                    .clone();

                $body
            },
        }
    };

    // Pattern for three argument function without type checking.
    ($debugger:ty, $name:expr, $res_type:expr, $arg_types:expr, |$ctx:ident, $param1:ident, $param2:ident, $param3:ident| $body:expr) => {
        FunctionDefinition::<$debugger> {
            name: $name.to_string(),
            argument_types: $arg_types,
            result_type: $res_type,
            definition: |$ctx, xs: &[TracedExpr<VariableId>]| {
                let $param1 = xs
                    .get(0)
                    .ok_or(EvaluationError::WrongNumberOfArguments {
                        expected: 3,
                        actual: 0,
                        for_function: $name.to_string(),
                    })?
                    .clone();

                let $param2 = xs
                    .get(1)
                    .ok_or(EvaluationError::WrongNumberOfArguments {
                        expected: 3,
                        actual: 1,
                        for_function: $name.to_string(),
                    })?
                    .clone();

                let $param3 = xs
                    .get(2)
                    .ok_or(EvaluationError::WrongNumberOfArguments {
                        expected: 3,
                        actual: 2,
                        for_function: $name.to_string(),
                    })?
                    .clone();

                $body
            },
        }
    };
}

pub(crate) use build_function;
