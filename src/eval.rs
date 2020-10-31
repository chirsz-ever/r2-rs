use crate::utils::*;
use anyhow::format_err;
use std::rc::Rc;
use AST::*;

pub fn interp(exp: &AST, env: &mut Env) -> anyhow::Result<RetValue> {
    match exp {
        Value(val) => Ok(val.clone()),
        Identifier(x) => Ok(env
            .lookup(&x)
            .ok_or_else(|| format_err!("undefined variable \"{}\"", x))?
            .clone()),
        LambdaDef { x, body } => Ok(RetValue::Procedure(func_from_ast(
            Rc::clone(&x),
            Rc::clone(&body),
            env.clone(),
        ))),
        Bind { x, e, body } => {
            let env_init = env.clone();
            let v1 = interp(e, env)?;
            *env = env_init;
            interp_arr(&body, &mut env.extend(x.clone(), v1))
        }
        Definition { x, e } => {
            let env_init = env.clone();
            let v1 = interp(e, env)?;
            *env = env_init.extend(x.clone(), v1);
            Ok(RetValue::Unit)
        }
        Block(es) => interp_arr(&es, env),
        Application { func, args } => {
            let env_init = env.clone();
            let v1 = interp(func, env)?;
            let mut argvs = Vec::new();
            for arg in args {
                argvs.push(interp(arg, env)?);
            }
            *env = env_init;
            match v1 {
                RetValue::Procedure(func) => func.call(&argvs),
                _ => anyhow::bail!("{} is not a procedure", v1),
            }
        }
    }
}

// None => Unit
// else => the last expression
fn interp_arr(es: &[AST], env: &mut Env) -> anyhow::Result<RetValue> {
    match es {
        [] => Ok(RetValue::Unit),
        [lead @ .., last] => {
            for e in lead {
                interp(e, env)?;
            }
            interp(last, env)
        }
    }
}

pub fn func_from_ast(x: Rc<str>, body: Rc<Vec<AST>>, env: Env) -> Function {
    Function::new(move |args| {
        if args.len() == 1 {
            let arg = args[0].clone();
            interp_arr(&body, &mut env.extend(x.clone(), arg))
        } else {
            anyhow::bail!("incorrect number of arguments")
        }
    })
}

#[cfg(test)]
pub mod test {
    use super::*;
    use RetValue::*;

    #[inline]
    pub fn num(n: i32) -> AST {
        Value(RetValue::num(n.into()))
    }

    #[inline]
    pub fn rvnum(n: i32) -> RetValue {
        RetValue::num(n.into())
    }

    #[inline]
    pub fn op(opc: &str, arg1: AST, arg2: AST) -> AST {
        let bf = match opc {
            "+" => builtin_plus,
            "-" => builtin_minus,
            "*" => builtin_multiply,
            "/" => builtin_divide,
            _ => unreachable!(),
        };
        Application {
            func: Rc::new(Value(Procedure(Function::new(bf)))),
            args: vec![arg1, arg2],
        }
    }

    #[inline]
    pub fn app(func: AST, arg: AST) -> AST {
        Application {
            func: Rc::new(func),
            args: vec![arg],
        }
    }

    #[inline]
    pub fn app_id(idnet: &str, arg: AST) -> AST {
        app(Identifier(idnet.into()), arg)
    }

    #[inline]
    pub fn def(x: &str, body: AST) -> AST {
        lambda(x, vec![body])
    }

    #[inline]
    pub fn bind(x: &str, e: AST, body: AST) -> AST {
        AST::Bind {
            x: x.into(),
            e: Rc::new(e),
            body: vec![body],
        }
    }

    #[inline]
    pub fn eval_eq(ast: AST, exp: RetValue) {
        use RetValue::*;
        let result = interp(&ast, &mut prelude_env()).unwrap();
        match (&result, &exp) {
            (Number(n1), Number(n2)) => assert_eq!(n1, n2),
            (Procedure(_), Procedure(_)) => todo!(),
            _ => panic!("{} is not equal to {}", result, exp),
        }
    }

    #[test]
    fn yin_1() {
        eval_eq(op("+", num(1), num(2)), rvnum(3));
    }

    #[test]
    fn yin_2() {
        eval_eq(op("*", num(2), num(3)), rvnum(6));
    }

    #[test]
    fn yin_3() {
        eval_eq(
            op("*", op("+", num(1), num(2)), op("+", num(3), num(4))),
            rvnum(21),
        );
    }

    #[test]
    fn yin_4() {
        eval_eq(app(def("x", op("*", num(2), var("x"))), num(3)), rvnum(6));
    }

    #[test]
    fn yin_5() {
        eval_eq(
            bind(
                "x",
                num(2),
                bind(
                    "f",
                    def("y", op("*", var("x"), var("y"))),
                    app(var("f"), num(3)),
                ),
            ),
            rvnum(6),
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
                    def("y", op("*", var("x"), var("y"))),
                    bind("x", num(4), app(var("f"), num(3))),
                ),
            ),
            rvnum(6),
        )
    }

    #[test]
    fn is_zero_1() {
        // (is_zero 0)
        eval_eq(
            app(app(app_id("is_zero", num(0)), num(0)), num(1)),
            rvnum(0),
        )
    }

    #[test]
    fn is_zero_2() {
        // (is_zero 2)
        eval_eq(
            app(app(app_id("is_zero", num(2)), num(0)), num(1)),
            rvnum(1),
        )
    }

    #[test]
    fn is_zero_3() {
        // (is_zero (lambda (x) x))
        eval_eq(
            app(app(app_id("is_zero", def("x", var("x"))), num(0)), num(1)),
            rvnum(1),
        )
    }

    #[test]
    fn church_true_test() {
        // (((is_zero 0) 1) 2)
        eval_eq(
            app(app(app_id("is_zero", num(0)), num(1)), num(2)),
            rvnum(1),
        )
    }

    #[test]
    fn church_false_test() {
        // (((is_zero 1) 1) 2)
        eval_eq(
            app(app(app_id("is_zero", num(1)), num(1)), num(2)),
            rvnum(2),
        )
    }
}
