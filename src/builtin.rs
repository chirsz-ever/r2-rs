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
