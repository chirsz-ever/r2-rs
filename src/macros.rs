macro_rules! fail_with_caller {
    ($name:expr, $msg:literal $(,)?) => {
        if let Some(name) = $name {
            anyhow::bail!(concat!("Exception in call `{}`: ", $msg), name);
        } else {
            anyhow::bail!(concat!("Exception: ", $msg));
        }
    };
    ($name:expr, $fmt:expr, $($arg:tt)*) => {
        if let Some(name) = $name {
            anyhow::bail!(concat!("Exception in call `{}`: ", $fmt), name, $($arg)*);
        } else {
            anyhow::bail!(concat!("Exception: ", $fmt), $($arg)*);
        }
    }
}

macro_rules! fail_if_nan {
    ($name:expr, $arg:expr) => {
        match $arg {
            RetValue::Number(_) => {}
            _ => {
                fail_with_caller!($name, "{} is not a number", $arg);
            }
        }
    };
}

macro_rules! fail_wrong_argc {
    ($name:expr) => {
        fail_with_caller!($name, "incorrect argument count");
    };
}

macro_rules! env_extend_args {
    ($func:ident, $e:ident) => {
        $crate::utils::RetValue::Procedure($crate::utils::Function::new($func))
    };
    ($func:literal, $e:ident) => {
        $crate::parse::parse_repl_input($func)
            .and_then(|ast| $crate::eval::interp(&ast, &mut $e))
            .unwrap()
    };
}

macro_rules! make_env {
    { $($name:expr => $value:tt),* $(,)? } => {
        {
            let mut e = $crate::utils::Env::new();
            $({
                    let v = env_extend_args!($value, e);
                    e = e.extend($name.into(), v);
            })*
            e
        }
    };
}
