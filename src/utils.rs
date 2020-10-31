pub use num_bigint::BigInt;
use num_traits::identities::Zero;
use std::fmt;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub enum AST {
    Identifier(Rc<str>),
    Number(BigInt),
    BuiltInFunc(Function),
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
    Unit,
}

pub fn lambda(x: &str, body: Vec<AST>) -> AST {
    AST::LambdaDef {
        x: x.into(),
        body: Rc::new(body),
    }
}

pub fn num(n: BigInt) -> AST {
    AST::Number(n)
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

#[derive(Clone)]
pub struct Function(pub Rc<dyn Fn(&[RetValue]) -> anyhow::Result<RetValue>>);

impl fmt::Debug for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "<built-in function {:p}>", &*self.0)
    }
}

impl Function {
    pub fn new(f: impl Fn(&[RetValue]) -> anyhow::Result<RetValue> + 'static) -> Self {
        Function(Rc::new(f) as _)
    }

    pub fn call(&self, args: &[RetValue]) -> anyhow::Result<RetValue> {
        self.0(args)
    }
}

// The "final" value
#[derive(Debug, Clone)]
pub enum RetValue {
    Number(BigInt),
    Procedure(Function),
    Unit,
}

impl fmt::Display for RetValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RetValue::Number(n) => write!(f, "{}", n),
            RetValue::Procedure(fun) => write!(f, "{:?}", fun),
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

pub fn builtin_plus(args: &[RetValue]) -> anyhow::Result<RetValue> {
    for arg in args {
        if let RetValue::Number(_) = arg {
        } else {
            anyhow::bail!("{} is not a number", arg)
        }
    }
    let mut sum = BigInt::from(0);
    for arg in args {
        if let RetValue::Number(n) = arg {
            sum += n;
        }
    }
    Ok(RetValue::Number(sum))
}

pub fn builtin_multiply(args: &[RetValue]) -> anyhow::Result<RetValue> {
    for arg in args {
        if let RetValue::Number(_) = arg {
        } else {
            anyhow::bail!("{} is not a number", arg)
        }
    }
    let mut prod = BigInt::from(1);
    for arg in args {
        if let RetValue::Number(n) = arg {
            prod *= n;
            if prod.is_zero() {
                return Ok(RetValue::Number(prod));
            }
        }
    }
    Ok(RetValue::Number(prod))
}

pub fn builtin_minus(args: &[RetValue]) -> anyhow::Result<RetValue> {
    for arg in args {
        if let RetValue::Number(_) = arg {
        } else {
            anyhow::bail!("{} is not a number", arg)
        }
    }
    match args {
        [] => anyhow::bail!("incorrect argument count"),
        [RetValue::Number(x)] => Ok(RetValue::Number(-x)),
        [RetValue::Number(x), subs @ ..] => {
            let mut ret = x.clone();
            for sub in subs {
                if let RetValue::Number(n) = sub {
                    ret -= n;
                }
            }
            Ok(RetValue::Number(ret))
        }
        _ => unreachable!(),
    }
}

pub fn builtin_divide(args: &[RetValue]) -> anyhow::Result<RetValue> {
    for arg in args {
        if let RetValue::Number(_) = arg {
        } else {
            anyhow::bail!("{} is not a number", arg)
        }
    }
    match args {
        [] => anyhow::bail!("incorrect argument count"),
        [RetValue::Number(x)] => Ok(RetValue::Number(1 / x)),
        [RetValue::Number(x), divs @ ..] => {
            let mut ret = x.clone();
            for div in divs {
                if let RetValue::Number(n) = div {
                    if n.is_zero() {
                        anyhow::bail!("0 is undefined to be divisor");
                    }
                    ret /= n;
                }
            }
            Ok(RetValue::Number(ret))
        }
        _ => unreachable!(),
    }
}

pub fn builtin_iszero(args: &[RetValue]) -> anyhow::Result<RetValue> {
    match args {
        [RetValue::Number(arg)] => {
            if arg.is_zero() {
                Ok(church_true())
            } else {
                Ok(church_false())
            }
        }
        [_] => Ok(church_false()),
        _ => anyhow::bail!("incorrect argument count"),
    }
}

pub fn prelude_env() -> Env {
    Env::new()
        .extend("+".into(), RetValue::Procedure(Function::new(builtin_plus)))
        .extend(
            "-".into(),
            RetValue::Procedure(Function::new(builtin_minus)),
        )
        .extend(
            "*".into(),
            RetValue::Procedure(Function::new(builtin_multiply)),
        )
        .extend(
            "/".into(),
            RetValue::Procedure(Function::new(builtin_divide)),
        )
        .extend(
            "is_zero".into(),
            RetValue::Procedure(Function::new(builtin_iszero)),
        )
}

use crate::eval::func_from_ast;
thread_local! {
    static CHURCH_TRUE: RetValue = RetValue::Procedure(func_from_ast(
        "x".into(),
        Rc::new(vec![lambda("y", vec![var("x")])]),
        Env::new(),
    ));

    static CHURCH_FALSE: RetValue = RetValue::Procedure(func_from_ast(
        "x".into(),
        Rc::new(vec![lambda("y", vec![var("y")])]),
        Env::new(),
    ));
}

pub fn church_true() -> RetValue {
    CHURCH_TRUE.with(|t| t.clone())
}

pub fn church_false() -> RetValue {
    CHURCH_FALSE.with(|f| f.clone())
}
