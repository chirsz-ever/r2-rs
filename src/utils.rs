
use std::fmt;
use nom::error::{VerboseError, convert_error as nom_convert_error};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AST {
    Symbol(String),
    Number(i32),
    LambdaDef {
        x: String,
        e: Box<AST>,
    },
    Bind {
        x: String,
        e1: Box<AST>,
        e2: Box<AST>,
    },
    Application {
        e1: Box<AST>,
        e2: Box<AST>,
    },
    BuiltInFunc {
        f: String,
        e: Box<AST>,
    },
    BuiltInOp {
        op: char,
        e1: Box<AST>,
        e2: Box<AST>,
    },
}

pub fn op(op: char, e1: AST, e2: AST) -> AST {
    AST::BuiltInOp {
        op,
        e1: Box::new(e1),
        e2: Box::new(e2),
    }
}

pub fn def(x: &str, e: AST) -> AST {
    AST::LambdaDef {
        x: x.to_string(),
        e: Box::new(e),
    }
}

pub fn num(n: i32) -> AST {
    AST::Number(n)
}

pub fn var(x: &str) -> AST {
    AST::Symbol(x.to_string())
}

pub fn bind(x: &str, e1: AST, e2: AST) -> AST {
    AST::Bind {
        x: x.to_string(),
        e1: Box::new(e1),
        e2: Box::new(e2),
    }
}

pub fn app(e1: AST, e2: AST) -> AST {
    AST::Application {
        e1: Box::new(e1),
        e2: Box::new(e2),
    }
}

pub fn func(f: &str, e: AST) -> AST {
    AST::BuiltInFunc {
        f: f.to_string(),
        e: Box::new(e),
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RetValue {
    Number(i32),
    Lambda(Closure),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Closure {
    pub f: Box<AST>,
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

pub type Env = Vec<(String, RetValue)>;

pub fn lookup<'a>(env: &'a Env, x: &str) -> Result<&'a RetValue, String> {
    env.iter()
        .rev()
        .find(|(s, _v)| s == x)
        .map(|(_, v)| v)
        .ok_or_else(|| format!("undefined variable {:?}", x))
}

pub fn ext_env(mut env: Env, x: String, v: RetValue) -> Env {
    env.push((x, v));
    env
}

pub fn env0() -> Env {
    Vec::new()
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
    fn from(info: String)-> Self {
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
