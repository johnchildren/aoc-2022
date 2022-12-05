mod check;
mod codegen;
mod eval;
mod syntax;

use std::{fs::File, io::Read, ops::Range};

use ariadne::{Report, ReportBuilder, ReportKind, Source};
use inkwell::{context::Context, execution_engine};

use crate::{
    eval::{eval, synth},
    syntax::{parser, Parser, Syntax}, codegen::CodeGen,
};

fn main() {
    //let src = "- + - 2 2";
    //let src = "readLines \"foo.txt\"";
    //let src = "parseInteger \"12\"";
    let src = "day1.yo";
    let mut file = File::open(src).unwrap();
    let mut buf = Vec::new();
    file.read_to_end(&mut buf);

    let (syntax, errs) = parser().parse_recovery(String::from_utf8(buf).unwrap());

    for err in errs {
        let report: ReportBuilder<Range<usize>> =
            Report::build(ReportKind::Error, (), err.span().start);

        let report = match err.reason() {
            chumsky::error::SimpleReason::Unexpected => report.with_message(format!(
                "{}, expected {}",
                if err.found().is_some() {
                    "Unexpected token in input"
                } else {
                    "Unexpected end of input"
                },
                if err.expected().len() == 0 {
                    "something else".to_string()
                } else {
                    err.expected()
                        .map(|expected| match expected {
                            Some(expected) => expected.to_string(),
                            None => "end of input".to_string(),
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                }
            )),
            _ => unimplemented!(),
        };

        report.finish().print(Source::from(&src)).unwrap();
    }
    let context = Context::create();
    let module = context.create_module("yomi");
    let execution_engine = module.create_jit_execution_engine(inkwell::OptimizationLevel::None).unwrap();
    let codegen = CodeGen {
        context: &context,
        module,
        builder: context.create_builder(),
        execution_engine,
    };

    println!("{:?}", syntax);
    match syntax {
        Some(Syntax::Expression(e)) => {
            println!("{:?}", synth(e.clone().into()));
            println!("{:?}", eval(e.clone().into()));

            let expr = codegen.jit_compile_expression(e.into());
            println!("{:?}", expr);
        }
        None => {}
    }
}
