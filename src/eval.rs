use crate::utils::*;
use anyhow::format_err;
use std::rc::Rc;
use RetValue::*;
use AST::{Application, Bind, BuiltInFunc, BuiltInOp, Identifier, LambdaDef};

pub fn interp(exp: &AST, env: &Env) -> anyhow::Result<RetValue> {
    match exp {
        Identifier(x) => Ok(env
            .lookup(&x)
            .ok_or_else(|| format_err!("undefined variable \"{}\"", x))?
            .clone()),
        AST::Number(n) => Ok(Number(n.clone())),
        LambdaDef { .. } => Ok(Lambda(Closure {
            f: Rc::new(exp.clone()),
            env: env.clone(),
        })),
        Bind { x, e1, e2 } => {
            let v1 = interp(e1, env)?;
            interp(e2, &env.extend(x.clone(), v1))
        }
        Application { e1, e2 } => {
            let v1 = interp(e1, env)?;
            let v2 = interp(e2, env)?;
            let v1info = format!("{:?}", v1);
            if let Lambda(Closure { f, env: env_save }) = v1 {
                if let LambdaDef { x, e } = &*f {
                    return interp(&e, &env_save.extend(x.clone(), v2));
                }
            }
            Err(format_err!("{} can't be function", v1info))
        }
        BuiltInOp { op, e1, e2 } => {
            let v1 = interp(e1, env)?;
            let v2 = interp(e2, env)?;
            match (v1, v2) {
                (Number(v1), Number(v2)) => Ok(Number(match *op {
                    '+' => v1 + v2,
                    '-' => v1 - v2,
                    '*' => v1 * v2,
                    '/' => v1 / v2,
                    _ => return Err(format_err!("{:?} is not a valid binary op", op)),
                })),
                _ => Err(format_err!("wrong argument")),
            }
        }
        BuiltInFunc { f, e } => {
            if "is_zero" == &**f {
                let v = interp(e, env)?;
                match v {
                    Number(ref n) if n == &0.into() => Ok(church_true()),
                    _ => Ok(church_false()),
                }
            } else {
                Err(format_err!("undefined function: {}", f))
            }
        }
    }
}

thread_local! {
    static CHURCH_TRUE: RetValue = Lambda(Closure {
        f: Rc::new(def("x", def("y", var("x")))),
        env: Env::new(),
    });

    static CHURCH_FALSE: RetValue = Lambda(Closure {
        f: Rc::new(def("x", def("y", var("y")))),
        env: Env::new(),
    });
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
