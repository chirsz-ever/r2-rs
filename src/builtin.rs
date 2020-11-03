use crate::utils::*;
use num::{Integer, Signed, Zero};

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

pub fn prelude_env() -> Env {
    make_env! {
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
}
