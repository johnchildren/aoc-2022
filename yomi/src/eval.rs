use std::{
    fs::File,
    io::{BufRead, Read},
};

use inkwell::{types::{BasicTypeEnum, IntType}, values::BasicValueEnum};

use crate::syntax;

#[derive(Debug, Clone)]
pub(crate) struct Assertion {
    t: Box<Expression>,
    e: Box<Expression>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub(crate) enum Builtin {
    Neg,
    Sub,
    Add,
    Mul,
    ReadLines,
    ParseInteger,
    SortIntegers,
    SumIntegers,
    Map,
    SplitVectorOn,
}

impl From<syntax::Builtin> for Builtin {
    fn from(builtin: syntax::Builtin) -> Self {
        match builtin {
            syntax::Builtin::Negate => Self::Neg,
            syntax::Builtin::ReadLines => Self::ReadLines,
            syntax::Builtin::ParseInteger => Self::ParseInteger,
            syntax::Builtin::SortIntegers => Self::SortIntegers,
            syntax::Builtin::SumIntegers => Self::SumIntegers,
            syntax::Builtin::Add => Self::Add,
            syntax::Builtin::Mul => Self::Mul,
            syntax::Builtin::Map => Self::Map,
            syntax::Builtin::SplitVectorOn => Self::SplitVectorOn,
        }
    }
}

impl From<syntax::Operation> for Builtin {
    fn from(op: syntax::Operation) -> Self {
        match op {
            syntax::Operation::Builtin(op) => op.into(),
        }
    }
}

fn builtin_type(builtin: Builtin) -> Value {
    match builtin {
        Builtin::Neg => Value::Arrow(vec![Value::IntegerType], Box::new(Value::IntegerType)),
        Builtin::Sub => Value::Arrow(
            vec![Value::IntegerType, Value::IntegerType],
            Box::new(Value::IntegerType),
        ),
        Builtin::Add => Value::Arrow(
            vec![Value::IntegerType, Value::IntegerType],
            Box::new(Value::IntegerType),
        ),
        Builtin::Mul => Value::Arrow(
            vec![Value::IntegerType, Value::IntegerType],
            Box::new(Value::IntegerType),
        ),
        Builtin::ReadLines => Value::Arrow(
            vec![Value::StringType],
            Box::new(Value::VectorType(Box::new(Value::StringType))),
        ),
        Builtin::ParseInteger => {
            Value::Arrow(vec![Value::StringType], Box::new(Value::IntegerType))
        }
        Builtin::SortIntegers => Value::Arrow(
            vec![Value::VectorType(Box::new(Value::IntegerType))],
            Box::new(Value::VectorType(Box::new(Value::IntegerType))),
        ),
        Builtin::SumIntegers => Value::Arrow(
            vec![Value::VectorType(Box::new(Value::IntegerType))],
            Box::new(Value::IntegerType),
        ),
        Builtin::Map => Value::Arrow(
            vec![
                Value::Arrow(vec![Value::IntegerType], Box::new(Value::IntegerType)),
                Value::VectorType(Box::new(Value::IntegerType)),
            ],
            Box::new(Value::VectorType(Box::new(Value::IntegerType))),
        ),
        Builtin::SplitVectorOn => Value::Arrow(
            vec![
                Value::StringType,
                Value::VectorType(Box::new(Value::StringType)),
            ],
            Box::new(Value::VectorType(Box::new(Value::VectorType(Box::new(
                Value::StringType,
            ))))),
        ),
    }
}

#[derive(Debug, Clone)]
pub struct Index(pub i64);

#[derive(Debug, Clone)]
pub(crate) enum Expression {
    Assertion(Assertion),
    Builtin(Builtin),
    Variable(Index),
    Lambda(Vec<Index>, Box<Expression>),
    Arrow(Vec<Expression>, Box<Expression>),
    Application(Box<Expression>, Vec<Expression>),
    Integer(i64),
    IntegerType,
    String(String),
    StringType,
    Vector(Vec<Expression>),
    VectorType(Box<Expression>),
    Universe, // Type of Type
}

impl From<syntax::Expression> for Expression {
    fn from(expr: syntax::Expression) -> Self {
        match expr {
            syntax::Expression::Integer(i) => Self::Integer(i),
            syntax::Expression::String(s) => Self::String(s),
            syntax::Expression::Operation(op) => Self::Builtin(op.into()),
            syntax::Expression::Application(op, es) => Self::Application(
                Box::new(Self::Builtin(op.into())),
                es.into_iter().map(Into::into).collect(),
            ),
            syntax::Expression::Parenthesis(e) => (*e).into(),
        }
    }
}

impl From<Value> for Expression {
    fn from(val: Value) -> Self {
        match val {
            Value::Builtin(b) => Self::Builtin(b),
            Value::Variable(v) => Self::Variable(v),
            Value::Closure(_) => unimplemented!(),
            Value::Arrow(args, ret) => Self::Arrow(
                args.into_iter().map(Into::into).collect(),
                Box::new((*ret).into()),
            ),
            Value::Integer(i) => Self::Integer(i),
            Value::IntegerType => Self::IntegerType,
            Value::String(s) => Self::String(s),
            Value::StringType => Self::StringType,
            Value::Vector(vs) => Self::Vector(vs.into_iter().map(Into::into).collect()),
            Value::VectorType(vt) => Self::VectorType(Box::new((*vt).into())),
            Value::Universe => Self::Universe,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Closure {
    // TODO: add environent
    pub args: Vec<Index>,
    pub body: Expression,
}

#[derive(Debug, Clone)]
pub(crate) enum Value {
    Builtin(Builtin),
    Variable(Index),
    Closure(Closure),
    Arrow(Vec<Value>, Box<Value>),
    Integer(i64),
    IntegerType,
    String(String),
    StringType,
    Vector(Vec<Value>),
    VectorType(Box<Value>),
    Universe, // Type of Type
}

pub(crate) fn eval(expr: Expression) -> Result<Value, ()> {
    let debug_expr = expr.clone();
    match expr {
        Expression::Assertion(Assertion { t, e }) => eval(*e),
        Expression::Builtin(builtin) => Ok(Value::Builtin(builtin)),
        // TODO: extract from context
        Expression::Variable(index) => Ok(Value::Variable(index)),
        // TODO: add context
        Expression::Lambda(args, body) => Ok(Value::Closure(Closure { args, body: *body })),
        Expression::Arrow(args, ret) => Ok(Value::Arrow(
            args.into_iter().map(eval).collect::<Result<Vec<_>, _>>()?,
            Box::new(eval(*ret)?),
        )),
        Expression::Application(rator, rands) => {
            match (
                eval(*rator)?,
                &rands.into_iter().map(eval).collect::<Result<Vec<_>, _>>()?[..],
            ) {
                (Value::Builtin(Builtin::Neg), [Value::Integer(i)]) => Ok(Value::Integer(-i)),
                (Value::Builtin(Builtin::Sub), [Value::Integer(i1), Value::Integer(i2)]) => {
                    Ok(Value::Integer(i1 - i2))
                }
                (Value::Builtin(Builtin::Add), [Value::Integer(i1), Value::Integer(i2)]) => {
                    Ok(Value::Integer(i1 + i2))
                }
                (Value::Builtin(Builtin::Mul), [Value::Integer(i1), Value::Integer(i2)]) => {
                    Ok(Value::Integer(i1 * i2))
                }
                (Value::Builtin(Builtin::ReadLines), [Value::String(file_path)]) => {
                    let mut contents = File::open(file_path).unwrap();
                    let mut buf = Vec::new();
                    contents.read_to_end(&mut buf).unwrap();
                    Ok(Value::Vector(
                        buf.lines().map(|res| Value::String(res.unwrap())).collect(),
                    ))
                }
                (Value::Builtin(Builtin::ParseInteger), [Value::String(integer)]) => {
                    Ok(Value::Integer(integer.parse().unwrap()))
                }
                (Value::Builtin(Builtin::SortIntegers), [Value::Vector(vs)]) => {
                    let mut integers = vs
                        .iter()
                        .filter_map(|v| match v {
                            Value::Integer(i) => Some(*i),
                            _ => panic!("not an int"),
                        })
                        .collect::<Vec<_>>();
                    integers.sort();
                    Ok(Value::Vector(
                        integers.into_iter().map(Value::Integer).collect(),
                    ))
                }
                // Only allow mapping builtins for now.
                (Value::Builtin(Builtin::Map), [b @ Value::Builtin(_), Value::Vector(vs)]) => {
                    Ok(Value::Vector(
                        vs.into_iter()
                            .cloned()
                            .map(|v| {
                                eval(Expression::Application(
                                    Box::new(b.clone().into()),
                                    vec![v.into()],
                                ))
                            })
                            .collect::<Result<Vec<_>, _>>()?,
                    ))
                }
                _ => {
                    dbg!(&debug_expr);
                    Err(())
                }
            }
        }
        Expression::String(s) => Ok(Value::String(s)),
        Expression::StringType => Ok(Value::StringType),
        Expression::Integer(i) => Ok(Value::Integer(i)),
        Expression::IntegerType => Ok(Value::IntegerType),
        Expression::Vector(vs) => Ok(Value::Vector(
            vs.into_iter().map(eval).collect::<Result<Vec<_>, _>>()?,
        )),
        Expression::VectorType(vt) => Ok(Value::VectorType(Box::new(eval(*vt)?))),
        Expression::Universe => Ok(Value::Universe),
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Annotation {
    t: Expression,
    e: Expression,
}

pub(crate) fn synth(expr: Expression) -> Result<Annotation, ()> {
    match &expr {
        Expression::Application(rator, rands) => match (
            &**rator,
            &rands
                .into_iter()
                .cloned()
                .map(synth)
                .collect::<Result<Vec<_>, _>>()?[..],
        ) {
            (Expression::Builtin(builtin), annotations) => {
                let bt = builtin_type(builtin.clone());
                match bt {
                    Value::Arrow(args, ret) => Ok(Annotation {
                        t: (*ret).into(),
                        e: expr,
                    }),
                    _ => unimplemented!(),
                }
            }
            _ => {
                dbg!(&expr);
                unimplemented!()
            }
        },
        Expression::Integer(_) => Ok(Annotation {
            t: Expression::IntegerType,
            e: expr,
        }),
        Expression::IntegerType => Ok(Annotation {
            t: Expression::Universe,
            e: expr,
        }),
        Expression::String(_) => Ok(Annotation {
            t: Expression::StringType,
            e: expr,
        }),
        Expression::StringType => Ok(Annotation {
            t: Expression::Universe,
            e: expr,
        }),
        Expression::Universe => Ok(Annotation {
            t: Expression::Universe,
            e: expr,
        }),
        Expression::Builtin(op) => Ok(Annotation {
            t: builtin_type((*op).clone()).into(),
            e: expr,
        }),
        _ => {
            dbg!(&expr);
            unimplemented!()
        }
    }
}

pub(crate) fn check(expr: Expression, value: Value) -> Result<Expression, ()> {
    match (&expr, &value) {
        (Expression::Application(rator, rands), Value::IntegerType) => {
            let e1_anno = synth((**rator).clone())?;
            let t_val = eval(e1_anno.t)?;
            // TODO: actually type check this
            Ok(e1_anno.e)
        }
        (Expression::Integer(_), Value::IntegerType) => Ok(expr),
        (Expression::IntegerType, Value::Universe) => Ok(expr),
        (Expression::String(_), Value::StringType) => Ok(expr),
        (Expression::StringType, Value::Universe) => Ok(expr),
        (Expression::Vector(vs), Value::VectorType(vt)) => {
            vs.into_iter()
                .map(|v| check(v.clone(), vt.as_ref().clone()))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(expr)
        }
        _ => unimplemented!(),
    }
}
