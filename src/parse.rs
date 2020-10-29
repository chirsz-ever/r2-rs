use crate::utils::*;

use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;

use std::rc::Rc;

#[derive(Parser)]
#[grammar = "r2.pest"]
struct R2Parser;

pub fn parse_program(src: &str) -> anyhow::Result<Vec<AST>> {
    let program = R2Parser::parse(Rule::Program, &src)?.next().unwrap();
    let mut asts = Vec::new();
    for sexpr in program.into_inner() {
        if let Rule::SExpr = sexpr.as_rule() {
            asts.push(sexpr_to_ast(sexpr)?);
        }
    }
    Ok(asts)
}

pub fn parse_expr(src: &str) -> anyhow::Result<AST> {
    let sexpr = R2Parser::parse(Rule::SingleSExpr, &src)?
        .next()
        .unwrap() // Here is SingleSExpr
        .into_inner()
        .next()
        .unwrap();
    sexpr_to_ast(sexpr)
}

fn sexpr_to_ast(sexpr: Pair<Rule>) -> anyhow::Result<AST> {
    let inner = sexpr.into_inner().next().unwrap();
    let ast = match inner.as_rule() {
        Rule::Integer => num(inner.as_str().parse()?),
        Rule::Identifier => var(inner.as_str()),
        r => {
            let mut inner = inner.into_inner();
            match r {
                Rule::LambdaDef => {
                    let x = inner.next().unwrap().as_str();
                    let body = collect_sexpr(inner)?;
                    lambda(x, body)
                }
                Rule::Bind => {
                    let x = inner.next().unwrap().as_str();
                    let e = sexpr_to_ast(inner.next().unwrap())?;
                    let body = collect_sexpr(inner)?;
                    bind(x, e, body)
                }
                Rule::Definition => {
                    let x = inner.next().unwrap().as_str();
                    let e = sexpr_to_ast(inner.next().unwrap())?;
                    AST::Definition {
                        x: x.into(),
                        e: Rc::new(e),
                    }
                }
                Rule::Block => {
                    let es = collect_sexpr(inner)?;
                    AST::Block(es)
                }
                Rule::Application => {
                    let func = sexpr_to_ast(inner.next().unwrap())?;
                    let args = collect_sexpr(inner)?;
                    AST::Application {
                        func: Rc::new(func),
                        args,
                    }
                }
                _ => unreachable!(),
            }
        }
    };
    Ok(ast)
}

fn collect_sexpr(ps: Pairs<Rule>) -> anyhow::Result<Vec<AST>> {
    let mut es = Vec::new();
    for sexpr in ps {
        es.push(sexpr_to_ast(sexpr)?);
    }
    Ok(es)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::eval::interp;
    use RetValue::*;

    fn eval_eq(exp: &str, v: RetValue) {
        let ast = parse_expr(exp.trim()).unwrap();
        let r = interp(&ast, &prelude_env()).unwrap();
        match (&r, &v) {
            (Number(n1), Number(n2)) => assert_eq!(n1, n2),
            (Closure { .. }, Closure { .. }) => todo!(),
            (BuiltInFunc { .. }, BuiltInFunc { .. }) => todo!(),
            _ => panic!("{} is not equal to {}", r, v),
        }
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
            Number(6.into()),
        );
    }

    #[test]
    fn is_zero_1() {
        eval_eq("(((is_zero 0) 0) 1)", Number(0.into()))
    }

    #[test]
    fn is_zero_2() {
        eval_eq("(((is_zero 2) 0) 1)", Number(1.into()))
    }

    #[test]
    fn is_zero_3() {
        eval_eq("(((is_zero (lambda (x) x)) 0) 1)", Number(1.into()))
    }

    #[test]
    fn church_true_test() {
        eval_eq("(((is_zero 0) 1) 2)", Number(1.into()))
    }

    #[test]
    fn church_false_test() {
        eval_eq("(((is_zero 1) 1) 2)", Number(2.into()))
    }
}
