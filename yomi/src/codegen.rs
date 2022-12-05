use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    module::Module,
    values::{BasicValueEnum, IntMathValue, IntValue},
};

use crate::eval::{self, Builtin, Expression, Value};

type NegateFunc = unsafe extern "C" fn(i64) -> i64;
type ReadLinesFunc = unsafe extern "C" fn(&str) -> &[&str];

pub(crate) struct CodeGen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,
}

impl<'ctx> CodeGen<'ctx> {
    fn jit_compile_builtin_application<'a>(
        &'a self,
        builtin: Builtin,
        args: Vec<BasicValueEnum<'a>>,
    ) -> Option<BasicValueEnum> {
        match (builtin, &args[..]) {
            (Builtin::Neg, [BasicValueEnum::IntValue(x)]) => Some(BasicValueEnum::IntValue(
                self.builder.build_int_neg(*x, "neg"),
            )),
            (Builtin::Sub, [BasicValueEnum::IntValue(x), BasicValueEnum::IntValue(y)]) => Some(
                BasicValueEnum::IntValue(self.builder.build_int_sub(*x, *y, "sub")),
            ),
            (Builtin::Add, [BasicValueEnum::IntValue(x), BasicValueEnum::IntValue(y)]) => Some(
                BasicValueEnum::IntValue(self.builder.build_int_add(*x, *y, "add")),
            ),
            (Builtin::Mul, [BasicValueEnum::IntValue(x), BasicValueEnum::IntValue(y)]) => Some(
                BasicValueEnum::IntValue(self.builder.build_int_mul(*x, *y, "mul")),
            ),
            _ => None,
        }
    }

    pub(crate) fn jit_compile_expression(&self, expr: Expression) -> Option<BasicValueEnum> {
        dbg!(&expr);
        match expr {
            Expression::Application(rator, rands) => match &*rator {
                Expression::Builtin(op) => self.jit_compile_builtin_application(
                    *op,
                    rands
                        .into_iter()
                        .map(|rand| self.jit_compile_expression(rand))
                        .collect::<Option<_>>()?,
                ),
                _ => {
                    None
                }
            },
            Expression::Integer(i) => {
                let int64_type = self.context.i64_type();
                Some(BasicValueEnum::IntValue(int64_type.const_int(i as u64, true)))
            }
            Expression::IntegerType => {
                let integer_type_struct = self.context.opaque_struct_type("Integer");
                Some(BasicValueEnum::StructValue(integer_type_struct.const_zero()))
            }
            _ => {
                dbg!(&expr);
                None
            }
        }
    }
}
