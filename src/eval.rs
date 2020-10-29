use crate::utils::*;
use anyhow::format_err;
use std::rc::Rc;
use AST::*;

pub fn interp(exp: &AST, env: &Env) -> anyhow::Result<RetValue> {
    match exp {
        Identifier(x) => Ok(env
            .lookup(&x)
            .ok_or_else(|| format_err!("undefined variable \"{}\"", x))?
            .clone()),
        AST::Number(n) => Ok(RetValue::Number(n.clone())),
        BuiltInFunc(func) => Ok(RetValue::BuiltInFunc(func.clone())),
        LambdaDef { x, e } => Ok(RetValue::Closure {
            arg: Rc::clone(x),
            expr: Rc::clone(e),
            env: env.clone(),
        }),
        Bind { x, e1, e2 } => {
            let v1 = interp(e1, env)?;
            interp(e2, &env.extend(x.clone(), v1))
        }
        Application { func, args } => {
            let v1 = interp(func, env)?;
            let mut argvs = Vec::new();
            for arg in args {
                argvs.push(interp(arg, env)?);
            }
            match v1 {
                RetValue::Closure {
                    ref arg,
                    ref expr,
                    env: ref env_save,
                } => {
                    if argvs.len() == 1 {
                        let v2 = argvs.into_iter().next().unwrap();
                        interp(&expr, &env_save.extend(arg.clone(), v2))
                    } else {
                        anyhow::bail!("incorrect number of arguments to {}", v1)
                    }
                }
                RetValue::BuiltInFunc(func) => func.0(&argvs),
                _ => anyhow::bail!("{} is not a procedure", v1),
            }
        }
    }
}

thread_local! {
    static CHURCH_TRUE: RetValue = RetValue::Closure {
        arg: "x".into(),
        expr: Rc::new(def("y", var("x"))),
        env: Env::new(),
    };

    static CHURCH_FALSE: RetValue = RetValue::Closure {
        arg: "x".into(),
        expr: Rc::new(def("y", var("y"))),
        env: Env::new(),
    };
}

pub fn church_true() -> RetValue {
    CHURCH_TRUE.with(|t| t.clone())
}

pub fn church_false() -> RetValue {
    CHURCH_FALSE.with(|f| f.clone())
}

#[cfg(test)]
mod test {
    use super::*;

    fn num(n: i32) -> AST {
        AST::Number(n.into())
    }

    fn eval_eq(ast: AST, exp: RetValue) {
        assert_eq!(interp(&ast, &Env::new()).unwrap(), exp);
    }

    #[test]
    fn yin_1() {
        eval_eq(op('+', num(1), num(2)), Number(3.into()));
    }

    #[test]
    fn yin_2() {
        eval_eq(op('*', num(2), num(3)), Number(6.into()));
    }

    #[test]
    fn yin_3() {
        eval_eq(
            op('*', op('+', num(1), num(2)), op('+', num(3), num(4))),
            Number(21.into()),
        );
    }

    #[test]
    fn yin_4() {
        eval_eq(
            app(def("x", op('*', num(2), var("x"))), num(3)),
            Number(6.into()),
        );
    }

    #[test]
    fn yin_5() {
        eval_eq(
            bind(
                "x",
                num(2),
                bind(
                    "f",
                    def("y", op('*', var("x"), var("y"))),
                    app(var("f"), num(3)),
                ),
            ),
            Number(6.into()),
        )
    }

    #[test]
    fn yin_6() {
        eval_eq(
            bind(
                "x",
                num(2),
                bind(
                    "f",
                    def("y", op('*', var("x"), var("y"))),
                    bind("x", num(4), app(var("f"), num(3))),
                ),
            ),
            Number(6.into()),
        )
    }

    #[test]
    fn is_zero_1() {
        // (is_zero 0)
        eval_eq(func("is_zero", num(0)), church_true())
    }

    #[test]
    fn is_zero_2() {
        // (is_zero 2)
        eval_eq(func("is_zero", num(2)), church_false())
    }

    #[test]
    fn is_zero_3() {
        // (is_zero (lambda (x) x))
        eval_eq(func("is_zero", def("x", var("x"))), church_false())
    }

    #[test]
    fn church_true_test() {
        // (((is_zero 0) 1) 2)
        eval_eq(
            app(app(func("is_zero", num(0)), num(1)), num(2)),
            Number(1.into()),
        )
    }

    #[test]
    fn church_false_test() {
        // (((is_zero 1) 1) 2)
        eval_eq(
            app(app(func("is_zero", num(1)), num(1)), num(2)),
            Number(2.into()),
        )
    }
}
