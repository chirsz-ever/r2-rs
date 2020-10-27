use nom::error::convert_error;
use num_bigint::BigInt;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AST {
    Identifier(Rc<str>),
    Number(BigInt),
    LambdaDef {
        x: Rc<str>,
        e: Rc<AST>,
    },
    Bind {
        x: Rc<str>,
        e1: Rc<AST>,
        e2: Rc<AST>,
    },
    Application {
        e1: Rc<AST>,
        e2: Rc<AST>,
    },
    BuiltInFunc {
        f: Rc<str>,
        e: Rc<AST>,
    },
    BuiltInOp {
        op: char,
        e1: Rc<AST>,
        e2: Rc<AST>,
    },
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

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Env(Option<Rc<EnvNode>>);

#[derive(Debug, Clone, PartialEq, Eq)]
struct EnvNode {
    kv: (Rc<str>, RetValue),
    next: Option<Rc<EnvNode>>,
}

impl Env {
    pub fn new() -> Env {
        Default::default()
    }

    pub fn lookup(&self, x: &str) -> Option<&RetValue> {
        let mut next = &self.0;
        while let Some(node) = next {
            if &*node.kv.0 == x {
                return Some(&node.kv.1);
            }
            next = &node.next;
        }
        None
    }

    pub fn extend(&self, x: Rc<str>, v: RetValue) -> Env {
        Env(Some(Rc::new(EnvNode {
            kv: (x, v),
            next: self.0.clone(),
        })))
    }
}

pub fn r2(exp: &str) -> anyhow::Result<RetValue> {
    use crate::eval::interp;
    use crate::parse::parse_r2;
    let ast = match parse_r2(&exp) {
        Ok(ast) => ast,
        Err(ve) => anyhow::bail!(convert_error(exp, ve)),
    };
    interp(&ast, &Env::new())
}
