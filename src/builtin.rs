use crate::utils::*;
use num::{pow::Pow, Integer, One, Signed, Zero};
use std::convert::TryFrom;

fn equal_q(name: Option<&str>, args: &[RetValue]) -> anyhow::Result<RetValue> {
    match args {
        [rv1, rv2] => Ok(RetValue::Boolean(rv1 == rv2)),
        _ => fail_wrong_argc!(name),
    }
}

fn eqv_q(name: Option<&str>, args: &[RetValue]) -> anyhow::Result<RetValue> {
    match args {
        [rv1, rv2] => Ok(RetValue::Boolean(rv1 == rv2)),
        _ => fail_wrong_argc!(name),
    }
}

fn eq_q(name: Option<&str>, args: &[RetValue]) -> anyhow::Result<RetValue> {
    match args {
        [rv1, rv2] => Ok(RetValue::Boolean(rv1 == rv2)),
        _ => fail_wrong_argc!(name),
    }
}

pub fn plus(name: Option<&str>, args: &[RetValue]) -> anyhow::Result<RetValue> {
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

pub fn multiply(name: Option<&str>, args: &[RetValue]) -> anyhow::Result<RetValue> {
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

pub fn minus(name: Option<&str>, args: &[RetValue]) -> anyhow::Result<RetValue> {
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

pub fn divide(name: Option<&str>, args: &[RetValue]) -> anyhow::Result<RetValue> {
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

fn number_q(name: Option<&str>, args: &[RetValue]) -> anyhow::Result<RetValue> {
    match args {
        [RetValue::Number(_)] => Ok(RetValue::Boolean(true)),
        [_] => Ok(RetValue::Boolean(false)),
        _ => fail_wrong_argc!(name),
    }
}

fn boolean_q(name: Option<&str>, args: &[RetValue]) -> anyhow::Result<RetValue> {
    match args {
        [RetValue::Boolean(_)] => Ok(RetValue::Boolean(true)),
        [_] => Ok(RetValue::Boolean(false)),
        _ => fail_wrong_argc!(name),
    }
}

fn procedure_q(name: Option<&str>, args: &[RetValue]) -> anyhow::Result<RetValue> {
    match args {
        [RetValue::Procedure(_)] => Ok(RetValue::Boolean(true)),
        [_] => Ok(RetValue::Boolean(false)),
        _ => fail_wrong_argc!(name),
    }
}

fn integer_q(name: Option<&str>, args: &[RetValue]) -> anyhow::Result<RetValue> {
    match args {
        [RetValue::Number(_)] => Ok(RetValue::Boolean(true)),
        [_] => Ok(RetValue::Boolean(false)),
        _ => fail_wrong_argc!(name),
    }
}

number_predicate! { zero_q,     n, n.is_zero()              }
number_predicate! { positive_q, n, n.is_positive()          }
number_predicate! { negative_q, n, n.is_negative()          }
number_predicate! { even_q,     n, n.is_even()              }
number_predicate! { odd_q,      n, n.is_odd()               }

number_binary_predicate! { num_eq, n1, n2, n1 == n2 }
number_binary_predicate! { num_lt, n1, n2, n1 <  n2 }
number_binary_predicate! { num_gt, n1, n2, n1 >  n2 }
number_binary_predicate! { num_le, n1, n2, n1 <= n2 }
number_binary_predicate! { num_ge, n1, n2, n1 >= n2 }

fn num_max(name: Option<&str>, args: &[RetValue]) -> anyhow::Result<RetValue> {
    if args.len() == 0 {
        fail_wrong_argc!(name);
    }
    for arg in args {
        fail_if_nan!(name, arg);
    }
    let max_n = args
        .iter()
        .map(|a| match a {
            RetValue::Number(n) => n,
            _ => unreachable!(),
        })
        .max()
        .unwrap();
    Ok(RetValue::Number(max_n.clone()))
}

fn num_min(name: Option<&str>, args: &[RetValue]) -> anyhow::Result<RetValue> {
    if args.len() == 0 {
        fail_wrong_argc!(name);
    }
    for arg in args {
        fail_if_nan!(name, arg);
    }
    let max_n = args
        .iter()
        .map(|a| match a {
            RetValue::Number(n) => n,
            _ => unreachable!(),
        })
        .min()
        .unwrap();
    Ok(RetValue::Number(max_n.clone()))
}

fn num_abs(name: Option<&str>, args: &[RetValue]) -> anyhow::Result<RetValue> {
    match args {
        [RetValue::Number(x)] => Ok(RetValue::num(x.abs())),
        [arg] => fail_nan!(name, arg),
        _ => fail_wrong_argc!(name),
    }
}

fn expt(name: Option<&str>, args: &[RetValue]) -> anyhow::Result<RetValue> {
    match args {
        [RetValue::Number(n1), RetValue::Number(n2)] => {
            let ret = if n2.is_zero() {
                // x^0 = 1
                RetValue::num(One::one())
            } else if n1.is_zero() {
                // 0^x = 0 (x != 0)
                RetValue::num(Zero::zero())
            } else if n1.is_one() {
                // 1^x = 1
                RetValue::num(One::one())
            } else if n2.is_negative() {
                // [x^neg] = 0 (neg < 0)
                RetValue::num(Zero::zero())
            } else if n2.is_one() {
                RetValue::Number(n2.clone())
            } else {
                let n1 = BigInt::clone(n1);
                let n2u = num::BigUint::try_from(BigInt::clone(n2))?;
                RetValue::num(n1.pow(n2u))
            };
            Ok(ret)
        }
        [RetValue::Number(_), arg2] => fail_nan!(name, arg2),
        [arg1, _] => fail_nan!(name, arg1),
        _ => fail_wrong_argc!(name),
    }
}

fn not(name: Option<&str>, args: &[RetValue]) -> anyhow::Result<RetValue> {
    match args {
        [RetValue::Boolean(false)] => Ok(RetValue::Boolean(true)),
        [_] => Ok(RetValue::Boolean(false)),
        _ => fail_wrong_argc!(name),
    }
}

pub fn prelude_env() -> Env {
    make_env! {
        "equal?"     => equal_q,
        "eqv?"       => eqv_q,
        "eq?"        => eq_q,
        "+"          => plus,
        "-"          => minus,
        "*"          => multiply,
        "/"          => divide,
        "number?"    => number_q,
        "boolean?"   => boolean_q,
        "procedure?" => procedure_q,
        "integer?"   => integer_q,
        "zero?"      => zero_q,
        "positive?"  => positive_q,
        "negative?"  => negative_q,
        "even?"      => even_q,
        "odd?"       => odd_q,
        "="          => num_eq,
        "<"          => num_lt,
        ">"          => num_gt,
        "<="         => num_le,
        ">="         => num_le,
        "max"        => num_max,
        "min"        => num_min,
        "abs"        => num_abs,
        "expt"       => expt,
        "not"        => not,
        "is_zero"    =>
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
    use crate::eval::test::{rvbool, rvnum};
    use crate::parse::test::eval_eq;

    #[inline]
    fn snum(s: &str) -> crate::utils::RetValue {
        crate::utils::RetValue::num(s.parse().unwrap())
    }

    #[test]
    fn equal_q_1() {
        eval_eq("(equal? 2 2)", rvbool(true));
        eval_eq("(equal? #t #t)", rvbool(true));
        eval_eq("(equal? 1 2)", rvbool(false));
        eval_eq("(equal? #t #f)", rvbool(false));
    }

    #[test]
    fn add_1() {
        eval_eq("(+)", rvnum(0));
        eval_eq("(+ 2)", rvnum(2));
        eval_eq("(+ 3 4)", rvnum(7));
        eval_eq("(+ 3 4 5)", rvnum(12));
    }

    #[test]
    fn minus_1() {
        eval_eq("(- 2)", rvnum(-2));
        eval_eq("(- 3 4)", rvnum(-1));
        eval_eq("(- 3 4 5)", rvnum(-6));
    }

    #[test]
    #[should_panic]
    fn minus_2() {
        eval_eq("(-)", rvnum(0));
    }

    #[test]
    fn mul_1() {
        eval_eq("(*)", rvnum(1));
        eval_eq("(* 2)", rvnum(2));
        eval_eq("(* 3 4)", rvnum(12));
        eval_eq("(* 3 4 5)", rvnum(60));
    }

    #[test]
    fn divide_1() {
        eval_eq("(/ 1)", rvnum(1));
        eval_eq("(/ 2)", rvnum(0));
        eval_eq("(/ 4 2)", rvnum(2));
        eval_eq("(/ 729 9 3)", rvnum(27));
    }

    #[test]
    #[should_panic]
    fn divide_2() {
        eval_eq("(/)", rvnum(0));
    }

    #[test]
    #[should_panic]
    fn divide_3() {
        eval_eq("(/ 0)", rvnum(0));
    }

    #[test]
    #[should_panic]
    fn divide_4() {
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

    #[test]
    fn min_1() {
        eval_eq("(min 1 2 3)", rvnum(1));
        eval_eq("(min 19 2 3)", rvnum(2));
        eval_eq("(min 123)", rvnum(123));
        eval_eq("(min 3 2 1 0)", rvnum(0));
    }

    #[test]
    #[should_panic]
    fn min_2() {
        eval_eq("(min)", rvnum(0));
    }

    #[test]
    #[should_panic]
    fn min_3() {
        eval_eq("(min #f 2 3)", rvnum(0));
    }

    #[test]
    fn max_1() {
        eval_eq("(max 1 2 3)", rvnum(3));
        eval_eq("(max 19 2 3)", rvnum(19));
        eval_eq("(max 123)", rvnum(123));
        eval_eq("(max 3 2 1 0)", rvnum(3));
    }

    #[test]
    #[should_panic]
    fn max_2() {
        eval_eq("(max)", rvnum(0));
    }

    #[test]
    #[should_panic]
    fn max_3() {
        eval_eq("(max 1 #t 3)", rvnum(0));
    }

    #[test]
    fn abs_1() {
        eval_eq("(abs 123)", rvnum(123));
        eval_eq("(abs 0)", rvnum(0));
        eval_eq("(abs -97)", rvnum(97));
    }

    #[test]
    fn expt_1() {
        eval_eq("(expt 0 0)", rvnum(1));
        eval_eq("(expt 0 1)", rvnum(0));
        eval_eq("(expt 2 3)", rvnum(8));
        eval_eq("(expt 1 -1)", rvnum(1));
        eval_eq("(expt 2 -1)", rvnum(0));
        eval_eq("(expt 32 23)", snum("41538374868278621028243970633760768"));
    }

    #[test]
    fn not_1() {
        eval_eq("(not #t)", rvbool(false));
        eval_eq("(not 3)", rvbool(false));
        eval_eq("(not #f)", rvbool(true));
        eval_eq("(not zero?)", rvbool(false));
    }
}
