pub use num_bigint::BigInt;
use std::fmt;
use std::rc::Rc;

#[derive(Clone)]
pub struct Function(pub Rc<dyn Fn(&[RetValue]) -> anyhow::Result<RetValue>>);

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:p}", &*self.0)
    }
}

#[derive(Debug, Clone)]
pub enum AST {
    Identifier(Rc<str>),
    Number(BigInt),
    BuiltInFunc(Function),
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
        func: Rc<AST>,
        args: Vec<AST>,
    },
}

pub fn def(x: &str, e: AST) -> AST {
    AST::LambdaDef {
        x: x.into(),
        e: Rc::new(e),
    }
}

pub fn num(n: BigInt) -> AST {
    AST::Number(n)
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

pub fn func(f: impl Fn(&[RetValue]) -> anyhow::Result<RetValue> + 'static) -> Function {
    Function(Rc::new(f) as _)
}

#[derive(Debug, Clone)]
pub enum RetValue {
    Number(BigInt),
    Closure {
        arg: Rc<str>,
        expr: Rc<AST>,
        env: Env,
    },
    BuiltInFunc(Function),
}

impl fmt::Display for RetValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RetValue::Number(n) => write!(f, "{}", n),
            RetValue::Closure { .. } | RetValue::BuiltInFunc(_) => {
                write!(f, "<function {:p}>", self)
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Env(Option<Rc<EnvNode>>);

#[derive(Debug, Clone)]
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
