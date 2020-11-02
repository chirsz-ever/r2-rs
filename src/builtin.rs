use crate::utils::*;
use num_traits::identities::Zero;

pub fn builtin_plus(name: Option<&str>, args: &[RetValue]) -> anyhow::Result<RetValue> {
    for arg in args {
        fail_if_nan!(&name, arg);
    }
    let mut sum = BigInt::from(0);
    for arg in args {
        if let RetValue::Number(n) = arg {
            sum += &**n;
        }
    }
    Ok(RetValue::num(sum))
}

pub fn builtin_multiply(name: Option<&str>, args: &[RetValue]) -> anyhow::Result<RetValue> {
    for arg in args {
        fail_if_nan!(&name, arg);
    }
    let mut prod = BigInt::from(1);
    for arg in args {
        if let RetValue::Number(n) = arg {
            prod *= &**n;
            if prod.is_zero() {
                return Ok(RetValue::num(prod));
            }
        }
    }
    Ok(RetValue::num(prod))
}

pub fn builtin_minus(name: Option<&str>, args: &[RetValue]) -> anyhow::Result<RetValue> {
    for arg in args {
        fail_if_nan!(&name, arg);
    }
    match args {
        [] => fail_wrong_argc!(name),
        [RetValue::Number(x)] => Ok(RetValue::num(-&**x)),
        [RetValue::Number(x), subs @ ..] => {
            let mut ret = BigInt::clone(&x);
            for sub in subs {
                if let RetValue::Number(n) = sub {
                    ret -= &**n;
                }
            }
            Ok(RetValue::num(ret))
        }
        _ => unreachable!(),
    }
}

pub fn builtin_divide(name: Option<&str>, args: &[RetValue]) -> anyhow::Result<RetValue> {
    for arg in args {
        fail_if_nan!(&name, arg);
    }
    match args {
        [] => fail_wrong_argc!(name),
        [RetValue::Number(x)] => Ok(RetValue::num(1 / &**x)),
        [RetValue::Number(x), divs @ ..] => {
            let mut ret = BigInt::clone(&x);
            for div in divs {
                if let RetValue::Number(n) = div {
                    if n.is_zero() {
                        fail_with_caller!(name, "0 is undefined to be divisor");
                    }
                    ret /= &**n;
                }
            }
            Ok(RetValue::num(ret))
        }
        _ => unreachable!(),
    }
}

pub fn builtin_number_q(name: Option<&str>, args: &[RetValue]) -> anyhow::Result<RetValue> {
    match args {
        [RetValue::Number(_)] => Ok(RetValue::Boolean(true)),
        [_] => Ok(RetValue::Boolean(false)),
        _ => fail_wrong_argc!(name),
    }
}

pub fn builtin_boolean_q(name: Option<&str>, args: &[RetValue]) -> anyhow::Result<RetValue> {
    match args {
        [RetValue::Boolean(_)] => Ok(RetValue::Boolean(true)),
        [_] => Ok(RetValue::Boolean(false)),
        _ => fail_wrong_argc!(name),
    }
}

pub fn builtin_procedure_q(name: Option<&str>, args: &[RetValue]) -> anyhow::Result<RetValue> {
    match args {
        [RetValue::Procedure(_)] => Ok(RetValue::Boolean(true)),
        [_] => Ok(RetValue::Boolean(false)),
        _ => fail_wrong_argc!(name),
    }
}

pub fn builtin_integer_q(name: Option<&str>, args: &[RetValue]) -> anyhow::Result<RetValue> {
    match args {
        [RetValue::Number(_)] => Ok(RetValue::Boolean(true)),
        [_] => Ok(RetValue::Boolean(false)),
        _ => fail_wrong_argc!(name),
    }
}

pub fn builtin_zero_q(name: Option<&str>, args: &[RetValue]) -> anyhow::Result<RetValue> {
    match args {
        [RetValue::Number(arg)] => Ok(RetValue::Boolean(arg.is_zero())),
        [arg] => fail_nan!(name, arg),
        _ => fail_wrong_argc!(name),
    }
}

pub fn prelude_env() -> Env {
    make_env! {
        "+" => builtin_plus,
        "-" => builtin_minus,
        "*" => builtin_multiply,
        "/" => builtin_divide,
        "zero?" => builtin_zero_q,
        "number?" => builtin_number_q,
        "boolean?" => builtin_boolean_q,
        "procedure?" => builtin_procedure_q,
        "integer?" => builtin_integer_q,
        "is_zero" =>
            "(lambda (n)
                (define tru (lambda (x) (lambda (y) x)))
                (define fls (lambda (x) (lambda (y) y)))
                (if (number? n)
                    (if (zero? n)
                        tru
                        fls)
                    fls))",
    }
}

#[cfg(test)]
mod test {
    use crate::eval::test::{rvnum, rvbool};
    use crate::parse::test::eval_eq;

    #[test]
    fn builtin_add_1() {
        eval_eq("(+)", rvnum(0));
        eval_eq("(+ 2)", rvnum(2));
        eval_eq("(+ 3 4)", rvnum(7));
        eval_eq("(+ 3 4 5)", rvnum(12));
    }

    #[test]
    fn builtin_minus_1() {
        eval_eq("(- 2)", rvnum(-2));
        eval_eq("(- 3 4)", rvnum(-1));
        eval_eq("(- 3 4 5)", rvnum(-6));
    }

    #[test]
    #[should_panic]
    fn builtin_minus_2() {
        eval_eq("(-)", rvnum(0));
    }

    #[test]
    fn builtin_mul_1() {
        eval_eq("(*)", rvnum(1));
        eval_eq("(* 2)", rvnum(2));
        eval_eq("(* 3 4)", rvnum(12));
        eval_eq("(* 3 4 5)", rvnum(60));
    }

    #[test]
    fn builtin_divide_1() {
        eval_eq("(/ 1)", rvnum(1));
        eval_eq("(/ 2)", rvnum(0));
        eval_eq("(/ 4 2)", rvnum(2));
        eval_eq("(/ 729 9 3)", rvnum(27));
    }

    #[test]
    #[should_panic]
    fn builtin_divide_2() {
        eval_eq("(/)", rvnum(0));
    }

    #[test]
    #[should_panic]
    fn builtin_divide_3() {
        eval_eq("(/ 0)", rvnum(0));
    }

    #[test]
    #[should_panic]
    fn builtin_divide_4() {
        eval_eq("(/ 4 0)", rvnum(0));
    }

    #[test]
    fn number_q_1() {
        eval_eq("(number? 0)", rvbool(true));
        eval_eq("(number? 1)", rvbool(true));
        eval_eq("(number? #t)", rvbool(false));
        eval_eq("(number? is_zero)", rvbool(false));
    }

    #[test]
    fn boolean_q_1() {
        eval_eq("(boolean? #t)", rvbool(true));
        eval_eq("(boolean? #f)", rvbool(true));
        eval_eq("(boolean? 0)", rvbool(false));
        eval_eq("(boolean? 1)", rvbool(false));
        eval_eq("(boolean? number?)", rvbool(false));
    }

    #[test]
    fn procedure_q_1() {
        eval_eq("(procedure? procedure?)", rvbool(true));
        eval_eq("(procedure? is_zero)", rvbool(true));
        eval_eq("(procedure? (lambda (x) x))", rvbool(true));
        eval_eq("(procedure? 1)", rvbool(false));
        eval_eq("(procedure? #t)", rvbool(false));
    }

    #[test]
    fn integer_q_1() {
        eval_eq("(integer? 0)", rvbool(true));
        eval_eq("(integer? 1)", rvbool(true));
        eval_eq("(integer? #t)", rvbool(false));
        eval_eq("(integer? is_zero)", rvbool(false));
    }

    #[test]
    fn zero_q_1() {
        eval_eq("(zero? 0)", rvbool(true));
        eval_eq("(zero? 1)", rvbool(false));
        eval_eq("(zero? 167)", rvbool(false));
    }

    #[test]
    #[should_panic]
    fn zero_q_2() {
        eval_eq("(zero? #t)", rvbool(true));
    }

    #[test]
    #[should_panic]
    fn zero_q_3() {
        eval_eq("(zero? is_zero)", rvbool(true));
    }
}
