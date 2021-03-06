pub use crate::builtin::prelude_env;
pub use num::BigInt;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum AST {
    Value(RetValue),
    Identifier(Rc<str>),
    LambdaDef {
        x: Rc<str>,
        body: Rc<Vec<AST>>,
    },
    Bind {
        x: Rc<str>,
        e: Rc<AST>,
        body: Vec<AST>,
    },
    Definition {
        x: Rc<str>,
        e: Rc<AST>,
    },
    Block(Vec<AST>),
    Application {
        func: Rc<AST>,
        args: Vec<AST>,
    },
    IfExpr {
        condition: Rc<AST>,
        then_branch: Rc<AST>,
        else_branch: Rc<AST>,
    },
}

pub fn lambda(x: &str, body: Vec<AST>) -> AST {
    AST::LambdaDef {
        x: x.into(),
        body: Rc::new(body),
    }
}

pub fn var(x: &str) -> AST {
    AST::Identifier(x.into())
}

pub fn bind(x: &str, e: AST, body: Vec<AST>) -> AST {
    AST::Bind {
        x: x.into(),
        e: Rc::new(e),
        body,
    }
}

type R2Function = dyn Fn(Option<&str>, &[RetValue]) -> anyhow::Result<RetValue>;

#[derive(Clone)]
pub struct Function(pub Rc<R2Function>);

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<procedure {:p}>", &*self.0)
    }
}

impl PartialEq<Self> for Function {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(
            &*self.0 as *const R2Function as *const u8,
            &*other.0 as *const R2Function as *const u8,
        )
    }
}

impl Function {
    #[inline]
    pub fn new(
        f: impl Fn(Option<&str>, &[RetValue]) -> anyhow::Result<RetValue> + 'static,
    ) -> Self {
        Function(Rc::new(f) as _)
    }

    #[inline]
    pub fn call(&self, name: Option<&str>, args: &[RetValue]) -> anyhow::Result<RetValue> {
        self.0(name, args)
    }
}

// The "final" value
#[derive(Debug, Clone, PartialEq)]
pub enum RetValue {
    Number(Rc<BigInt>),
    Boolean(bool),
    Procedure(Function),
    Unit,
}

impl RetValue {
    #[inline]
    pub fn num(n: BigInt) -> Self {
        RetValue::Number(Rc::new(n))
    }
}

impl fmt::Display for RetValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RetValue::Number(n) => write!(f, "{}", n),
            RetValue::Procedure(fun) => write!(f, "{:?}", fun),
            RetValue::Boolean(b) => {
                if *b {
                    write!(f, "#t")
                } else {
                    write!(f, "#f")
                }
            }
            RetValue::Unit => write!(f, ""),
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
