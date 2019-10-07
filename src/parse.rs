use crate::utils::*;
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::*,
    combinator::*,
    error::{context, VerboseError},
    multi::many0,
    sequence::*,
    IResult,
};

type Result<'a, O = AST> = IResult<&'a str, O, VerboseError<&'a str>>;

pub fn parse_r2(i: &str) -> std::result::Result<AST, VerboseError<&str>> {
    use nom::Err::*;
    parse_expr(i.trim())
        .map(|(_, ast)| ast)
        .map_err(|err| match err {
            Error(e) | Failure(e) => e,
            Incomplete(_) => unreachable!(),
        })
}

fn parse_expr(i: &str) -> Result<'_> {
    alt((
        parse_symbol,
        parse_number,
        parse_def,
        parse_bind,
        parse_builtin_func,
        parse_app,
        parse_builtin_op,
    ))(i)
}

fn in_paren<'a, O1, F>(inner: F) -> impl Fn(&'a str) -> Result<'a, O1>
where
    F: Fn(&'a str) -> Result<'a, O1>,
{
    delimited(
        char('('),
        sp(inner),
        context("closing paren", cut(sp(char(')')))),
    )
}

fn sp<'a, O1, F>(after: F) -> impl Fn(&'a str) -> Result<'a, O1>
where
    F: Fn(&'a str) -> Result<'a, O1>,
{
    preceded(multispace0, after)
}

fn sp1<'a, O1, F>(after: F) -> impl Fn(&'a str) -> Result<'a, O1>
where
    F: Fn(&'a str) -> Result<'a, O1>,
{
    preceded(multispace1, after)
}

fn parse_name(i: &str) -> Result<'_, &str> {
    let head = verify(anychar, |c| c.is_ascii_alphabetic() || *c == '_');
    let tail = verify(anychar, |c| c.is_ascii_alphanumeric() || *c == '_');
    recognize(pair(head, many0(tail)))(i)
}

fn parse_symbol(i: &str) -> Result<'_> {
    map(parse_name, var)(i)
}

fn parse_number(i: &str) -> Result<'_> {
    map_res(recognize(pair(opt(char('-')), digit1)), |s: &str| {
        s.parse::<i32>()
    })(i)
    .map(|(i, n)| (i, num(n)))
}

fn parse_def(i: &str) -> Result<'_> {
    let def_inner = map(
        tuple((
            tag("lambda"),
            cut(sp(in_paren(parse_name))),
            cut(sp(parse_expr)),
        )),
        |(_, arg, e)| def(arg, e),
    );
    in_paren(def_inner)(i)
}

fn parse_bind(i: &str) -> Result<'_> {
    let in_bracket = |inner1, inner2| {
        delimited(
            char('['),
            sp(separated_pair(inner1, multispace1, inner2)),
            context("closing bracket", cut(sp(char(']')))),
        )
    };
    let bind_inner = map(
        tuple((
            tag("let"),
            cut(sp(in_paren(in_bracket(parse_name, parse_expr)))),
            cut(sp(parse_expr)),
        )),
        |(_, (sym, e1), e2)| bind(sym, e1, e2),
    );
    in_paren(bind_inner)(i)
}

fn parse_app(i: &str) -> Result<'_> {
    let app_inner = pair(parse_expr, sp1(parse_expr));
    map(in_paren(app_inner), |(e1, e2)| app(e1, e2))(i)
}

fn parse_builtin_op(i: &str) -> Result<'_> {
    let app_inner = tuple((anychar, sp1(parse_expr), sp1(parse_expr)));
    map(in_paren(app_inner), |(o, e1, e2)| op(o, e1, e2))(i)
}

fn parse_builtin_func(i: &str) -> Result<'_> {
    let is_zero_inner = pair(tag("is_zero"), sp1(parse_expr));
    map(in_paren(is_zero_inner), |(f, e)| func(f, e))(i)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::eval::interp;
    use RetValue::*;

    fn eval_eq(exp: &str, v: RetValue) {
        let (_, ast) = parse_expr(exp.trim()).unwrap();
        let r = interp(&ast, &env0()).unwrap();
        assert_eq!(r, v);
    }

    #[test]
    fn yin_1() {
        eval_eq(r"(+ 1 2)", Number(3.into()));
    }

    #[test]
    fn yin_2() {
        eval_eq(r"(* 2 3)", Number(6.into()));
    }

    #[test]
    fn yin_3() {
        eval_eq(r"(* 2 (+ 3 4))", Number(14.into()));
    }

    #[test]
    fn yin_4() {
        eval_eq(r"(* (+ 1 2) (+ 3 4))", Number(21.into()));
    }

    #[test]
    fn yin_5() {
        eval_eq(r"((lambda (x) (* 2 x)) 3)", Number(6.into()));
    }

    #[test]
    fn yin_6() {
        eval_eq(
            r"
(let ([x 2])
   (let ([f (lambda (y) (* x y))])
     (f 3)))",
            Number(6.into()),
        );
    }

    #[test]
    fn yin_7() {
        eval_eq(
            r"
(let ([x 2])
   (let ([f (lambda (y) (* x y))])
     (let ([x 4])
       (f 3))))",
            Number(6),
        );
    }

    use crate::eval::{church_false, church_true};

    #[test]
    fn is_zero_1() {
        eval_eq("(is_zero 0)", church_true())
    }

    #[test]
    fn is_zero_2() {
        eval_eq("(is_zero 2)", church_false())
    }

    #[test]
    fn is_zero_3() {
        eval_eq("(is_zero (lambda (x) x))", church_false())
    }

    #[test]
    fn church_true_test() {
        eval_eq("(((is_zero 0) 1) 2)", Number(1))
    }

    #[test]
    fn church_false_test() {
        eval_eq("(((is_zero 1) 1) 2)", Number(2))
    }
}
