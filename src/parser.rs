use nom::bytes::streaming::take_while;
use nom::character::complete::satisfy;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{alphanumeric1, char, digit1, multispace0},
    combinator::{map, opt, recognize},
    multi::{many0, separated_list0},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};
use nom_locate::LocatedSpan;
use serde::{Deserialize, Serialize};

type Span<'a> = LocatedSpan<&'a str>;

#[derive(PartialEq, Eq, Debug, Clone, Copy, Serialize, Deserialize)]
pub struct CodeLocation {
    pub line: u32,
    pub column: usize,
    pub offset: usize,
}

impl From<Span<'_>> for CodeLocation {
    fn from(span: Span) -> Self {
        CodeLocation {
            line: span.location_line(),
            column: span.get_column(),
            offset: span.location_offset(),
        }
    }
}

fn lambda_params(input: Span) -> IResult<Span, Vec<String>> {
    map(
        opt(terminated(
            separated_list0(ws(char(',')), identifier),
            ws(tag("->")),
        )),
        |maybe_params| maybe_params.unwrap_or_default(),
    )(input)
}

fn lambda_body(input: Span) -> IResult<Span, Vec<Statement<String>>> {
    terminated(
        separated_list0(
            ws(char(';')),
            alt((
                let_statement,
                map(expression, |expr| Statement {
                    location: input.into(),
                    var: None,
                    expr: expr,
                }),
            )),
        ),
        opt(ws(char(';'))),
    )(input)
}

fn lambda_expr(input: Span) -> IResult<Span, RawExpr<String>> {
    map(
        delimited(
            ws(char('{')),
            tuple((lambda_params, lambda_body)),
            ws(char('}')),
        ),
        |(params, body)| {
            // Get the last statement from the body
            let (statements, return_expr) =
                if let Some((last, rest)) = body.split_last() {
                    if last.var.is_none() {
                        (rest.to_vec(), last.expr.clone())
                    } else {
                        (body, RawExpr::Unit)
                    }
                } else {
                    (body, RawExpr::Unit)
                };

            RawExpr::Lambda(params, statements, Box::new(return_expr))
        },
    )(input)
}

use crate::ast::{RawExpr, Statement};
use crate::types::ExprType;

// Utility parsers
fn ws<'a, F: 'a, O>(inner: F) -> impl FnMut(Span<'a>) -> IResult<Span<'a>, O>
where
    F: FnMut(Span<'a>) -> IResult<Span<'a>, O>,
{
    delimited(
        delimited(multispace0, opt(comment), multispace0),
        inner,
        delimited(multispace0, opt(comment), multispace0),
    )
}

fn comment(input: Span) -> IResult<Span, ()> {
    alt((
        block_comment,
        map(
            tuple((tag("//"), take_while1(|c| c != '\n'), opt(char('\n')))),
            |_| (),
        ),
    ))(input)
}

fn block_comment(input: Span) -> IResult<Span, ()> {
    let (input, _) = tag("/*")(input)?;
    let mut depth = 1;
    let mut pos = 0;

    while pos < input.len() - 1 {
        if &input[pos..pos + 2] == "/*" {
            depth += 1;
            pos += 2;
        } else if &input[pos..pos + 2] == "*/" {
            depth -= 1;
            if depth == 0 {
                return Ok((Span::new(&input[pos + 2..]), ()));
            }
            pos += 2;
        } else {
            pos += 1;
        }
    }

    Err(nom::Err::Error(nom::error::Error::new(
        input,
        nom::error::ErrorKind::Tag,
    )))
}

// Identifier parser
fn identifier(input: Span) -> IResult<Span, String> {
    map(
        recognize(pair(
            satisfy(|c| c.is_ascii_lowercase()),
            many0(alt((alphanumeric1, tag("_")))),
        )),
        |s: Span| s.fragment().to_string(),
    )(input)
}

// Type parsers
fn type_expr(input: Span) -> IResult<Span, ExprType> {
    alt((type_arrow, non_arrow_type))(input)
}

fn type_arrow(input: Span) -> IResult<Span, ExprType> {
    let (input, first) = non_arrow_type(input)?;
    let (input, rest) = many0(preceded(ws(tag("->")), non_arrow_type))(input)?;

    // Build up the function type chain from right to left
    let result = rest.into_iter().rev().fold(first, |acc, arg_type| {
        ExprType::Func(vec![arg_type], Box::new(acc))
    });

    Ok((input, result))
}

fn non_arrow_type(input: Span) -> IResult<Span, ExprType> {
    alt((
        map(tag("Int"), |_| ExprType::Int),
        map(tag("String"), |_| ExprType::String),
        map(tag("Bool"), |_| ExprType::Bool),
        map(tag("Float"), |_| ExprType::Float),
        map(tag("Type"), |_| ExprType::Type),
        map(tag("Unit"), |_| ExprType::Unit),
        map(tag("Any"), |_| ExprType::Any),
        map(
            preceded(
                tag("Pair"),
                delimited(
                    ws(char('(')),
                    tuple((type_expr, preceded(ws(char(',')), type_expr))),
                    ws(char(')')),
                ),
            ),
            |(t1, t2)| ExprType::Pair(Box::new(t1), Box::new(t2)),
        ),
        map(
            preceded(
                tag("Node"),
                delimited(ws(char('(')), type_expr, ws(char(')'))),
            ),
            |t| ExprType::Node(Box::new(t)),
        ),
        delimited(ws(char('(')), type_expr, ws(char(')'))),
    ))(input)
}

// Symbol parser
fn symbol(input: Span) -> IResult<Span, String> {
    alt((
        delimited(char('`'), identifier, char('`')),
        map(
            recognize(many0(alt((
                char('='),
                char('+'),
                char('-'),
                char('*'),
                char('/'),
                char('.'),
                char('&'),
                char('|'),
                char('!'),
                char('%'),
            )))),
            |s: Span| s.fragment().to_string(),
        ),
    ))(input)
}

// Literal parsers
fn float(input: Span) -> IResult<Span, RawExpr<String>> {
    map(
        tuple((recognize(digit1), char('.'), recognize(digit1))),
        |(int_digits, _, frac_digits): (Span, char, Span)| {
            RawExpr::Float(
                (int_digits.fragment().to_string().to_owned()
                    + "."
                    + frac_digits.fragment())
                .parse()
                .unwrap(),
            )
        },
    )(input)
}

fn integer(input: Span) -> IResult<Span, RawExpr<String>> {
    map(recognize(digit1), |digits: Span| {
        RawExpr::Int(digits.parse().unwrap())
    })(input)
}

fn string_literal(input: Span) -> IResult<Span, RawExpr<String>> {
    map(
        delimited(char('"'), take_while(|c| c != '"'), char('"')),
        |s: Span| RawExpr::String(s.to_string()),
    )(input)
}

fn boolean(input: Span) -> IResult<Span, RawExpr<String>> {
    alt((
        map(tag("true"), |_| RawExpr::Bool(true)),
        map(tag("false"), |_| RawExpr::Bool(false)),
    ))(input)
}

fn literal(input: Span) -> IResult<Span, RawExpr<String>> {
    alt((
        float,
        integer,
        string_literal,
        boolean,
        map(tag("None"), |_| RawExpr::None),
        map(tag("()"), |_| RawExpr::Unit),
    ))(input)
}

// Expression parsers
fn primary_expr(input: Span) -> IResult<Span, RawExpr<String>> {
    alt((
        ws(literal),
        map(ws(type_expr), RawExpr::Type),
        map(ws(identifier), RawExpr::Var),
        delimited(ws(char('(')), expression, ws(char(')'))),
    ))(input)
}

fn function_call(input: Span) -> IResult<Span, RawExpr<String>> {
    let (input, func) = identifier(input)?;

    // Handle both cases: with parentheses (possibly with args) and without
    let (input, args) = function_invocation(input)?;

    Ok((
        input,
        RawExpr::Apply(Box::new(RawExpr::Var(func)), args.into()),
    ))
}

fn function_invocation(input: Span) -> IResult<Span, Vec<RawExpr<String>>> {
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
    };

    Ok((input, final_args))
}

fn binary_expr(input: Span) -> IResult<Span, RawExpr<String>> {
    let (input, first) = non_binary_expression(input)?;

    let (input, rest) =
        many0(tuple((ws(symbol), non_binary_expression)))(input)?;

    // Build up the binary expression chain
    let result = rest.into_iter().fold(first, |acc, (sym, right)| {
        RawExpr::Apply(
            Box::new(RawExpr::Var(sym.to_string())),
            vec![acc, right].into_boxed_slice(),
        )
    });

    Ok((input, result))
}

fn method_call(input: Span) -> IResult<Span, RawExpr<String>> {
    let (input, initial) = primary_expr(input)?;

    // Parse zero or more method calls
    let (input, method_chains) = many0(tuple((
        preceded(ws(char('.')), identifier),
        function_invocation,
    )))(input)?;

    // Transform the chain of method calls into nested function calls
    let result =
        method_chains
            .into_iter()
            .fold(initial, |acc, (method, args)| {
                // Create a new argument list with the receiver as the first argument
                let mut full_args = vec![acc];
                full_args.extend(args);

                // Construct the function call expression
                RawExpr::Apply(
                    Box::new(RawExpr::Var(method)),
                    full_args.into_boxed_slice(),
                )
            });

    Ok((input, result))
}

fn expression(input: Span) -> IResult<Span, RawExpr<String>> {
    alt((binary_expr, non_binary_expression))(input)
}

fn non_binary_expression(input: Span) -> IResult<Span, RawExpr<String>> {
    alt((lambda_expr, function_call, method_call, primary_expr))(input)
}

// Statement parsers
fn let_statement(input: Span) -> IResult<Span, Statement<String>> {
    map(
        tuple((ws(tag("let")), ws(identifier), ws(char('=')), expression)),
        |(_, id, _, expr)| Statement {
            location: input.into(),
            var: Some(id),
            expr: expr,
        },
    )(input)
}

pub fn parse_program(input: Span) -> IResult<Span, Vec<Statement<String>>> {
    // A full program is basically a top-level lambda we're executing.
    lambda_body(input)
}

pub fn parse(input: &str) -> Result<Vec<Statement<String>>, String> {
    match parse_program(Span::new(input)) {
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
    assert!(literal(Span::new("42")).is_ok());
    assert!(literal(Span::new("4.2")).is_ok());
    assert!(literal(Span::new("\"hello\"")).is_ok());
    assert!(literal(Span::new("true")).is_ok());
    assert!(literal(Span::new("false")).is_ok());
    assert!(literal(Span::new("None")).is_ok());
    assert!(literal(Span::new("()")).is_ok());
    assert!(expression(Span::new("Int")).is_ok());
}

#[test]
fn test_function_calls() {
    assert!(expression(Span::new("foo()")).is_ok());
    assert!(expression(Span::new("bar(1, 2)")).is_ok());
    assert!(expression(Span::new("baz(\"hello\", 42, true)")).is_ok());
}

#[test]
fn test_function_type() {
    assert!(expression(Span::new("Int -> Int")).is_ok());
    assert!(expression(Span::new("Int -> Int -> String")).is_ok());
}

#[test]
fn test_method_calls() {
    assert!(expression(Span::new("foo.bar(x)")).is_ok());
    assert!(expression(Span::new("foo.bar(x, y).baz(z)")).is_ok());
    assert!(expression(Span::new("node.value()")).is_ok());
}

#[test]
fn test_eq_calls() {
    assert!(expression(Span::new("x == 42")).is_ok());
    assert!(expression(Span::new("node.value == 42")).is_ok());
}

#[test]
fn test_lambda() {
    let input = r#"{ 42 }"#;
    assert!(expression(Span::new(input)).is_ok());

    let input = "{ x -> x + 2 }";

    assert!(expression(Span::new(input)).is_ok());
}

#[test]
fn test_multiline_lambda() {
    let input = r#"{
        println("x");
        println("y")
    }"#;
    assert!(expression(Span::new(input)).is_ok());
}

#[test]
fn test_launch_lambda() {
    let input = r#"launch {
        println("x");
        println("y");
    }"#;
    assert!(expression(Span::new(input)).is_ok());
}

#[test]
fn test_await() {
    let input = r#"task.await()"#;
    assert!(expression(Span::new(input)).is_ok());
}

#[test]
fn test_assert() {
    assert!(expression(Span::new("assert(node.value == 0)")).is_ok());
    assert!(
        let_statement(Span::new("let x = assert(node.value == 0);")).is_ok()
    );
}

#[test]
fn test_statements() {
    assert!(let_statement(Span::new("let x = 42;")).is_ok());
    assert!(expression(Span::new("foo()")).is_ok());
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
