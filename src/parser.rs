use nom::character::complete::newline;
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
            delimited(multispace0, char(';'), multispace0),
            alt((
                let_statement,
                map(expression, |expr| Statement::Execute(None, expr.traced())),
            )),
        ),
        opt(delimited(multispace0, char(';'), multispace0)),
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
            alt((alpha1, tag("_"))),
            many0(alt((alphanumeric1, tag("_")))),
        )),
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
        literal,
        map(identifier, Expr::Var),
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

fn member_access(input: &str) -> IResult<&str, Expr> {
    let (input, initial) = primary_expr(input)?;

    let (input, accesses) = many0(preceded(ws(char('.')), identifier))(input)?;

    let result = accesses.into_iter().fold(initial, |expr, member| {
        // Placeholder implementation
        Expr::None
    });

    Ok((input, result))
}

fn comparison_expr(input: &str) -> IResult<&str, Expr> {
    let (input, first) = non_cmp_expression(input)?;

    let (input, rest) =
        many0(tuple((ws(tag("==")), non_cmp_expression)))(input)?;

    // Build up the binary expression chain
    let result = rest.into_iter().fold(first, |acc, (_, right)| {
        Expr::Apply(
            Box::new(Expr::Var("==".to_string()).traced()),
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
    alt((comparison_expr, non_cmp_expression))(input)
}

fn non_cmp_expression(input: &str) -> IResult<&str, Expr> {
    alt((
        lambda_expr,
        function_call,
        member_access,
        method_call,
        primary_expr,
    ))(input)
}

// Statement parsers
fn let_statement(input: &str) -> IResult<&str, Statement> {
    map(
        tuple((ws(tag("let")), ws(identifier), ws(char('=')), expression)),
        |(_, id, _, expr)| Statement::Var(id, expr.traced()),
    )(input)
}

pub fn parse_program(input: &str) -> IResult<&str, Vec<Statement>> {
    terminated(
        separated_list0(
            delimited(multispace0, char(';'), multispace0),
            let_statement,
        ),
        delimited(multispace0, char(';'), multispace0),
    )(input)
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
}

#[test]
fn test_function_calls() {
    assert!(expression("foo()").is_ok());
    assert!(expression("bar(1, 2)").is_ok());
    assert!(expression("baz(\"hello\", 42, true)").is_ok());
}

#[test]
fn test_member_access() {
    assert!(expression("foo.bar").is_ok());
    assert!(expression("foo.bar.baz").is_ok());
    assert!(expression("node.value").is_ok());
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
        let input = r#"
            let x = 42;

            let f = {
                print_ln("Hello, world!");
                let value = node.value;
                assert(value == 0);
            };

            let g = {
                print_ln("Hello, world!");
                let value = node.value;
                assert(value == 0);
            };
            "#;

        let result = parse(input);
        dbg!(parse(input));
        assert!(result.is_ok());
    }
}
