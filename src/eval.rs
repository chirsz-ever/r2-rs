use crate::utils::*;
use RetValue::*;
use AST::{Application, Bind, BuiltInFunc, BuiltInOp, LambdaDef, Symbol};

pub fn interp(exp: &AST, env: &Env) -> Result<RetValue, String> {
    match exp {
        Symbol(x) => Ok(lookup(&env, &x)?.clone()),
        AST::Number(n) => Ok(Number(*n)),
        LambdaDef { .. } => Ok(Lambda(Closure {
            f: Box::new(exp.clone()),
            env: env.clone(),
        })),
        Bind { x, e1, e2 } => {
            let v1 = interp(e1, env)?;
            interp(e2, &ext_env(env, x.clone(), v1))
        }
        Application { e1, e2 } => {
            let v1 = interp(e1, env)?;
            let v2 = interp(e2, env)?;
            let v1info = format!("{:?}", v1);
            if let Lambda(Closure { f, env: env_save }) = v1 {
                if let LambdaDef { x, e } = *f {
                    return interp(&e, &ext_env(&env_save, x, v2));
                }
            }
            Err(format!("{} can't be function", v1info))
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
                    _ => return Err(format!("{:?} is not a valid binary op", op)),
                })),
                _ => Err("wrong argument".to_string()),
            }
        }
        BuiltInFunc { f, e } => {
            if f == "is_zero" {
                let v = interp(e, env)?;
                if let Number(0) = v {
                    Ok(church_true())
                } else {
                    Ok(church_false())
                }
            } else {
                Err(format!("undefined function: {}", f))
            }
        }
    }
}

pub fn church_true() -> RetValue {
    Lambda(Closure {
        f: Box::new(def("x", def("y", var("x")))),
        env: env0(),
    })
}

pub fn church_false() -> RetValue {
    Lambda(Closure {
        f: Box::new(def("x", def("y", var("y")))),
        env: env0(),
    })
}

#[cfg(test)]
mod test {
    use super::*;

    fn eval_eq(ast: AST, exp: RetValue) {
        assert_eq!(interp(&ast, &env0()).unwrap(), exp);
    }

    #[test]
    fn yin_1() {
        eval_eq(op('+', num(1), num(2)), Number(3));
    }

    #[test]
    fn yin_2() {
        eval_eq(op('*', num(2), num(3)), Number(6));
    }

    #[test]
    fn yin_3() {
        eval_eq(
            op('*', op('+', num(1), num(2)), op('+', num(3), num(4))),
            Number(21),
        );
    }

    #[test]
    fn yin_4() {
        eval_eq(app(def("x", op('*', num(2), var("x"))), num(3)), Number(6));
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
            Number(6),
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
            Number(6),
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
        eval_eq(app(app(func("is_zero", num(0)), num(1)), num(2)), Number(1))
    }

    #[test]
    fn church_false_test() {
        // (((is_zero 1) 1) 2)
        eval_eq(app(app(func("is_zero", num(1)), num(1)), num(2)), Number(2))
    }
}
