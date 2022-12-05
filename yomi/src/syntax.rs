pub use chumsky::Parser;
use chumsky::{
    prelude::Simple,
    primitive::{choice, filter, just},
    recursive::recursive,
    text,
};

#[derive(Debug, Clone)]
pub(crate) enum Builtin {
    Negate,
    ReadLines,
    ParseInteger,
    SortIntegers,
    SumIntegers,
    Add,
    Mul,
    Map,
    SplitVectorOn,
}

#[derive(Debug, Clone)]
pub(crate) enum Operation {
    Builtin(Builtin),
}

#[derive(Debug, Clone)]
pub(crate) enum Expression {
    Application(Operation, Vec<Expression>),
    Operation(Operation),
    String(String),
    Integer(i64),
    Parenthesis(Box<Expression>),
}

#[derive(Debug, Clone)]
pub(crate) enum Syntax {
    Expression(Expression),
}

pub(crate) fn parser() -> impl Parser<char, Syntax, Error = Simple<char>> {
    recursive(|expr| {
        let integer = just('-')
            .or_not()
            .chain::<char, _, _>(text::int(10))
            .collect::<String>()
            .from_str()
            .unwrapped()
            .map(Expression::Integer);

        let string = just('"')
            .ignore_then(filter(|c| *c != '"').repeated())
            .then_ignore(just('"'))
            .collect::<String>()
            .map(Expression::String);

        let operation = choice((
            just('-').to(Builtin::Negate),
            just("readLines").to(Builtin::ReadLines),
            just("parseInteger").to(Builtin::ParseInteger),
            just("sortIntegers").to(Builtin::SortIntegers),
            just("sumIntegers").to(Builtin::SumIntegers),
            just('+').to(Builtin::Add),
            just('*').to(Builtin::Mul),
            just("map").to(Builtin::Map),
            just("splitVectorOn").to(Builtin::SplitVectorOn),
        ))
        .map(Operation::Builtin);

        let application = operation.clone()
            .then(just(' ').ignore_then(expr.clone()).repeated().at_least(1))
            .map(|(op, es)| Expression::Application(op, es));

        let parens = expr
            .delimited_by(just('('), just(')'))
            .map(|e| Expression::Parenthesis(Box::new(e)));

        choice((parens, application, operation.map(Expression::Operation), integer, string))
    })
    .map(Syntax::Expression)
}
