use nom::error::{convert_error as nom_convert_error, VerboseError};
use num_bigint::BigInt;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AST {
    Identifier(Rc<str>),
    Number(BigInt),
    LambdaDef { x: Rc<str>, e: Rc<AST> },
    Bind { x: Rc<str>, e1: Rc<AST>, e2: Rc<AST> },
    Application { e1: Rc<AST>, e2: Rc<AST> },
    BuiltInFunc { f: Rc<str>, e: Rc<AST> },
    BuiltInOp { op: char, e1: Rc<AST>, e2: Rc<AST> },
}

pub fn op(op: char, e1: AST, e2: AST) -> AST {
    AST::BuiltInOp {
        op,
        e1: Rc::new(e1),
        e2: Rc::new(e2),
    }
}

pub fn def(x: &str, e: AST) -> AST {
    AST::LambdaDef {
        x: x.into(),
        e: Rc::new(e),
    }
}

pub fn num(n: i32) -> AST {
    AST::Number(n.into())
}

pub fn var(x: &str) -> AST {
    AST::Identifier(x.into())
}

pub fn bind(x: &str, e1: AST, e2: AST) -> AST {
    AST::Bind {
        x: x.into(),
        e1: Rc::new(e1),
        e2: Rc::new(e2),
    }
}

pub fn app(e1: AST, e2: AST) -> AST {
    AST::Application {
        e1: Rc::new(e1),
        e2: Rc::new(e2),
    }
}

pub fn func(f: &str, e: AST) -> AST {
    AST::BuiltInFunc {
        f: f.into(),
        e: Rc::new(e),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RetValue {
    Number(BigInt),
    Lambda(Closure),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Closure {
    pub f: Rc<AST>,
    pub env: Env,
}

impl fmt::Display for RetValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RetValue::Number(n) => write!(f, "{}", n),
            RetValue::Lambda(c) => write!(f, "<function {:p}>", c),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Env(Option<Rc<EnvNode>>);

#[derive(Debug, Clone, PartialEq, Eq)]
struct EnvNode {
    kv: (Rc<str>, RetValue),
    next: Option<Rc<EnvNode>>,
}

pub fn lookup<'a>(env: &'a Env, x: &str) -> Result<&'a RetValue, String> {
    let mut next = &env.0;
    while let Some(node) = next {
        if &*node.kv.0 == x {
            return Ok(&node.kv.1);
        }
        next = &node.next;
    }
    Err(format!("undefined variable \"{}\"", x))
}

pub fn ext_env(env: &Env, x: Rc<str>, v: RetValue) -> Env {
    Env(Some(Rc::new(EnvNode {
        kv: (x, v),
        next: env.0.clone(),
    })))
}

pub fn env0() -> Env {
    Env(None)
}

#[derive(Debug, Clone)]
pub enum R2Error<'a> {
    ParseError(VerboseError<&'a str>),
    RuntimeError(String),
}

impl<'a> From<VerboseError<&'a str>> for R2Error<'a> {
    fn from(e: VerboseError<&'a str>) -> R2Error<'a> {
        R2Error::ParseError(e)
    }
}

impl From<String> for R2Error<'_> {
    fn from(info: String) -> Self {
        R2Error::RuntimeError(info)
    }
}

// FIXME: nom bug (https://github.com/Geal/nom/issues/1027)
fn convert_error(data: &str, e: VerboseError<&str>) -> String {
    let backup = std::panic::take_hook();
    std::panic::set_hook(Box::new(|_| ()));
    let msg = std::panic::catch_unwind(|| nom_convert_error(&data, e))
        .unwrap_or_else(|_| String::from("Early End"));
    std::panic::set_hook(backup);
    msg
}

pub fn err_info(data: &str, e: R2Error<'_>) -> String {
    match e {
        R2Error::ParseError(ve) => convert_error(data, ve),
        R2Error::RuntimeError(re) => re,
    }
}

pub fn r2(exp: &str) -> Result<RetValue, R2Error<'_>> {
    use crate::eval::interp;
    use crate::parse::parse_r2;
    interp(&parse_r2(&exp)?, &env0()).map_err(R2Error::from)
}
