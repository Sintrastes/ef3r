use nom::character::complete::{newline, satisfy};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{
        alpha1, alphanumeric1, char, digit1, multispace0, multispace1,
    },
    combinator::{map, opt, recognize},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};

fn lambda_params(input: &str) -> IResult<&str, Vec<String>> {
    map(
        opt(terminated(
            separated_list0(ws(char(',')), identifier),
            ws(tag("->")),
        )),
        |maybe_params| maybe_params.unwrap_or_default(),
    )(input)
}

fn lambda_body(input: &str) -> IResult<&str, Vec<Statement>> {
    terminated(
        separated_list0(
            ws(char(';')),
            alt((
                let_statement,
                map(expression, |expr| Statement {
                    var: None,
                    expr: expr,
                }),
            )),
        ),
        opt(ws(char(';'))),
    )(input)
}

fn lambda_expr(input: &str) -> IResult<&str, Expr> {
    map(
        delimited(
            ws(char('{')),
            tuple((lambda_params, lambda_body)),
            ws(char('}')),
        ),
        |(params, body)| {
            Expr::Lambda(params, body, Box::new(Expr::Unit.traced()))
        },
    )(input)
}

use crate::ast::{Expr, Statement};
use crate::types::ExprType;

// Utility parsers
fn ws<'a, F: 'a, O>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O>
where
    F: FnMut(&'a str) -> IResult<&'a str, O>,
{
    delimited(multispace0, inner, multispace0)
}

// Identifier parser
fn identifier(input: &str) -> IResult<&str, String> {
    map(
        recognize(pair(
            satisfy(|c| c.is_ascii_lowercase()),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        String::from,
    )(input)
}

// Type parsers
fn type_expr(input: &str) -> IResult<&str, ExprType> {
    alt((type_arrow, non_arrow_type))(input)
}

fn type_arrow(input: &str) -> IResult<&str, ExprType> {
    let (input, first) = non_arrow_type(input)?;
    let (input, rest) = many0(preceded(ws(tag("->")), non_arrow_type))(input)?;

    // Build up the function type chain from right to left
    let result = rest.into_iter().rev().fold(first, |acc, arg_type| {
        ExprType::Func(vec![arg_type], Box::new(acc))
    });

    Ok((input, result))
}

fn non_arrow_type(input: &str) -> IResult<&str, ExprType> {
    alt((
        map(tag("Int"), |_| ExprType::Int),
        map(tag("String"), |_| ExprType::String),
        map(tag("Bool"), |_| ExprType::Bool),
        map(tag("Float"), |_| ExprType::Float),
        map(tag("Type"), |_| ExprType::Type),
        map(tag("Unit"), |_| ExprType::Unit),
        map(tag("Any"), |_| ExprType::Any),
        delimited(ws(char('(')), type_expr, ws(char(')'))),
    ))(input)
}

// Symbol parser
fn symbol(input: &str) -> IResult<&str, String> {
    map(
        recognize(many0(alt((
            char('='),
            char('+'),
            char('-'),
            char('*'),
            char('/'),
            char('.'),
        )))),
        String::from,
    )(input)
}

// Literal parsers
fn integer(input: &str) -> IResult<&str, Expr> {
    map(recognize(digit1), |digits: &str| {
        Expr::Int(digits.parse().unwrap())
    })(input)
}

fn string_literal(input: &str) -> IResult<&str, Expr> {
    map(
        delimited(char('"'), take_while1(|c| c != '"'), char('"')),
        |s: &str| Expr::String(s.to_string()),
    )(input)
}

fn boolean(input: &str) -> IResult<&str, Expr> {
    alt((
        map(tag("true"), |_| Expr::Bool(true)),
        map(tag("false"), |_| Expr::Bool(false)),
    ))(input)
}

fn literal(input: &str) -> IResult<&str, Expr> {
    alt((
        integer,
        string_literal,
        boolean,
        map(tag("None"), |_| Expr::None),
        map(tag("Unit"), |_| Expr::Unit),
    ))(input)
}

// Expression parsers
fn primary_expr(input: &str) -> IResult<&str, Expr> {
    alt((
        ws(literal),
        map(ws(type_expr), Expr::Type),
        map(ws(identifier), Expr::Var),
        delimited(ws(char('(')), expression, ws(char(')'))),
    ))(input)
}

fn function_call(input: &str) -> IResult<&str, Expr> {
    let (input, func) = identifier(input)?;

    // Handle both cases: with parentheses (possibly with args) and without
    let (input, (args, trailing_lambda)) = alt((
        // Case 1: f(arg1, arg2) { ... }
        map(
            tuple((
                delimited(
                    ws(char('(')),
                    separated_list0(ws(char(',')), expression),
                    ws(char(')')),
                ),
                opt(ws(lambda_expr)),
            )),
            |(args, lambda)| (args, lambda),
        ),
        // Case 2: f { ... }  (just lambda, no parens)
        map(ws(lambda_expr), |lambda| (vec![], Some(lambda))),
    ))(input)?;

    // Combine regular args with trailing lambda if present
    let mut final_args = args;
    if let Some(lambda) = trailing_lambda {
        final_args.push(lambda);
    }

    Ok((
        input,
        Expr::Apply(
            Box::new(Expr::Var(func).traced()),
            final_args
                .into_iter()
                .map(|e| e.traced())
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        ),
    ))
}

fn binary_expr(input: &str) -> IResult<&str, Expr> {
    let (input, first) = non_binary_expression(input)?;

    let (input, rest) =
        many0(tuple((ws(symbol), non_binary_expression)))(input)?;

    // Build up the binary expression chain
    let result = rest.into_iter().fold(first, |acc, (sym, right)| {
        Expr::Apply(
            Box::new(Expr::Var(sym.to_string()).traced()),
            vec![acc.traced(), right.traced()].into_boxed_slice(),
        )
    });

    Ok((input, result))
}

fn method_call(input: &str) -> IResult<&str, Expr> {
    let (input, initial) = primary_expr(input)?;

    // Parse zero or more method calls
    let (input, method_chains) = many0(tuple((
        preceded(ws(char('.')), identifier),
        delimited(
            ws(char('(')),
            separated_list0(ws(char(',')), expression),
            ws(char(')')),
        ),
    )))(input)?;

    // Transform the chain of method calls into nested function calls
    let result =
        method_chains
            .into_iter()
            .fold(initial, |acc, (method, args)| {
                // Create a new argument list with the receiver as the first argument
                let mut full_args = vec![acc.traced()];
                full_args.extend(args.into_iter().map(|e| e.traced()));

                // Construct the function call expression
                Expr::Apply(
                    Box::new(Expr::Var(method).traced()),
                    full_args.into_boxed_slice(),
                )
            });

    Ok((input, result))
}

fn expression(input: &str) -> IResult<&str, Expr> {
    alt((binary_expr, non_binary_expression))(input)
}

fn non_binary_expression(input: &str) -> IResult<&str, Expr> {
    alt((lambda_expr, function_call, method_call, primary_expr))(input)
}

// Statement parsers
fn let_statement(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((ws(tag("let")), ws(identifier), ws(char('=')), expression)),
        |(_, id, _, expr)| Statement {
            var: Some(id),
            expr: expr,
        },
    )(input)
}

pub fn parse_program(input: &str) -> IResult<&str, Vec<Statement>> {
    // A full program is basically a top-level lambda we're executing.
    lambda_body(input)
}

pub fn parse(input: &str) -> Result<Vec<Statement>, String> {
    match parse_program(input) {
        Ok((remaining, program)) => {
            if remaining.trim().is_empty() {
                Ok(program)
            } else {
                Err(format!(
                    "Parser did not consume all input. Remaining: {}",
                    remaining
                ))
            }
        }
        Err(e) => Err(format!("Parse error: {}", e)),
    }
}
#[test]
fn test_literal_expressions() {
    assert!(literal("42").is_ok());
    assert!(literal("\"hello\"").is_ok());
    assert!(literal("true").is_ok());
    assert!(literal("false").is_ok());
    assert!(literal("None").is_ok());
    assert!(literal("Unit").is_ok());
    assert!(expression("Int").is_ok());
}

#[test]
fn test_function_calls() {
    assert!(expression("foo()").is_ok());
    assert!(expression("bar(1, 2)").is_ok());
    assert!(expression("baz(\"hello\", 42, true)").is_ok());
}

#[test]
fn test_method_calls() {
    assert!(expression("foo.bar(x)").is_ok());
    assert!(expression("foo.bar(x, y).baz(z)").is_ok());
    assert!(expression("node.value()").is_ok());
}

#[test]
fn test_eq_calls() {
    assert!(expression("x == 42").is_ok());
    assert!(expression("node.value == 42").is_ok());
}

#[test]
fn test_lambda() {
    let input = r#"{ 42 }"#;
    assert!(expression(input).is_ok());
}

#[test]
fn test_multiline_lambda() {
    let input = r#"{
        println("x");
        println("y")
    }"#;
    assert!(expression(input).is_ok());
}

#[test]
fn test_launch_lambda() {
    let input = r#"launch {
        println("x");
        println("y");
    }"#;
    assert!(expression(input).is_ok());
}

#[test]
fn test_await() {
    let input = r#"task.await()"#;
    assert!(expression(input).is_ok());
}

#[test]
fn test_assert() {
    assert!(expression("assert(node.value == 0)").is_ok());
    assert!(let_statement("let x = assert(node.value == 0);").is_ok());
}

#[test]
fn test_statements() {
    assert!(let_statement("let x = 42;").is_ok());
    assert!(expression("foo()").is_ok());
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_original_syntax() {
        let input = r#"let f = {
                println("Hello, world!");
                let value = node.value();
            };"#;

        let result = parse(input);
        assert!(result.is_ok());
    }
}
