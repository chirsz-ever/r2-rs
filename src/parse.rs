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
        match sexpr.as_rule() {
            Rule::SExpr => asts.push(sexpr_to_ast(sexpr)?),
            Rule::Definition => asts.push(def_to_ast(sexpr)?),
            _ => {} // ignore
        }
    }
    Ok(asts)
}

pub fn parse_repl_input(src: &str) -> anyhow::Result<AST> {
    let input_inner = R2Parser::parse(Rule::REPLInput, &src)?
        .next()
        .unwrap() // Here is REPLInput
        .into_inner()
        .next()
        .unwrap();
    match input_inner.as_rule() {
        Rule::SExpr => sexpr_to_ast(input_inner),
        Rule::Definition => def_to_ast(input_inner),
        _ => Ok(AST::Value(RetValue::Unit)),
    }
}

fn sexpr_to_ast(sexpr: Pair<Rule>) -> anyhow::Result<AST> {
    let inner = sexpr.into_inner().next().unwrap();
    let ast = match inner.as_rule() {
        Rule::Integer => AST::Value(RetValue::num(inner.as_str().parse()?)),
        Rule::Identifier => var(inner.as_str()),
        r => {
            let mut inner = inner.into_inner();
            match r {
                Rule::LambdaDef => {
                    let x = inner.next().unwrap().as_str();
                    let body = collect_block_body(inner)?;
                    lambda(x, body)
                }
                Rule::Bind => {
                    let x = inner.next().unwrap().as_str();
                    let e = sexpr_to_ast(inner.next().unwrap())?;
                    let body = collect_block_body(inner)?;
                    bind(x, e, body)
                }
                Rule::Block => {
                    let es = collect_block_body(inner)?;
                    AST::Block(es)
                }
                Rule::Application => {
                    let func = sexpr_to_ast(inner.next().unwrap())?;
                    let args = collect_block_body(inner)?; // assert all are SExpr
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

fn def_to_ast(definition: Pair<Rule>) -> anyhow::Result<AST> {
    let mut inner = definition.into_inner();
    let x = inner.next().unwrap().as_str();
    let e = sexpr_to_ast(inner.next().unwrap())?;
    let ast = AST::Definition {
        x: x.into(),
        e: Rc::new(e),
    };
    Ok(ast)
}

#[inline]
fn collect_block_body(ps: Pairs<Rule>) -> anyhow::Result<Vec<AST>> {
    let mut es = Vec::new();
    for s in ps {
        match s.as_rule() {
            Rule::SExpr => es.push(sexpr_to_ast(s)?),
            Rule::Definition => es.push(def_to_ast(s)?),
            _ => unreachable!(),
        }
    }
    Ok(es)
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::eval::test::*;

    #[inline]
    fn eval_eq(exp: &str, v: RetValue) {
        let ast = parse_repl_input(exp.trim()).unwrap();
        crate::eval::test::eval_eq(ast, v);
    }

    #[test]
    fn yin_1() {
        eval_eq(r"(+ 1 2)", rvnum(3));
    }

    #[test]
    fn yin_2() {
        eval_eq(r"(* 2 3)", rvnum(6));
    }

    #[test]
    fn yin_3() {
        eval_eq(r"(* 2 (+ 3 4))", rvnum(14));
    }

    #[test]
    fn yin_4() {
        eval_eq(r"(* (+ 1 2) (+ 3 4))", rvnum(21));
    }

    #[test]
    fn yin_5() {
        eval_eq(r"((lambda (x) (* 2 x)) 3)", rvnum(6));
    }

    #[test]
    fn yin_6() {
        eval_eq(
            r"
(let ([x 2])
   (let ([f (lambda (y) (* x y))])
     (f 3)))",
            rvnum(6),
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
            rvnum(6),
        );
    }

    #[test]
    fn is_zero_1() {
        eval_eq("(((is_zero 0) 0) 1)", rvnum(0))
    }

    #[test]
    fn is_zero_2() {
        eval_eq("(((is_zero 2) 0) 1)", rvnum(1))
    }

    #[test]
    fn is_zero_3() {
        eval_eq("(((is_zero (lambda (x) x)) 0) 1)", rvnum(1))
    }

    #[test]
    fn church_true_test() {
        eval_eq("(((is_zero 0) 1) 2)", rvnum(1))
    }

    #[test]
    fn church_false_test() {
        eval_eq("(((is_zero 1) 1) 2)", rvnum(2))
    }
}
