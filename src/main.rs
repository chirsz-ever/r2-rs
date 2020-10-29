#![allow(dead_code)]

#[macro_use]
extern crate clap;

mod eval;
mod parse;
mod repl;
mod utils;

use crate::eval::*;
use crate::parse::*;
use crate::utils::*;
use std::fs::File;
use std::io::{self, prelude::*};

#[derive(Debug)]
enum Status {
    REPL,
    EVAL(String),
}

use Status::*;

fn main() -> io::Result<()> {
    let mut env = prelude_env();
    match deal_arg()? {
        Some(REPL) => repl::repl()?,
        Some(EVAL(src)) => match parse_program(&src) {
            Ok(asts) => {
                for ast in asts.into_iter() {
                    match interp(&ast, &mut env) {
                        Ok(ret) => println!("{}", ret),
                        Err(e) => eprintln!("Error: \n{}", e),
                    }
                }
            }
            Err(e) => eprintln!("Error: \n{}", e),
        },
        None => (),
    }

    Ok(())
}

fn deal_arg() -> io::Result<Option<Status>> {
    let matches = clap_app!(r2c =>
        (author: "Chris Ever. <chirsz@foxmail.com>")
        (about: "R2 Interpreter Implemented with Rust")
        (@arg INPUT: conflicts_with[CMD] "Sets the input file to use")
        (@arg REPL: -i conflicts_with[INPUT CMD] "Interactive REPL mode")
        (@arg expression: -c +takes_value "Expression passed in as string")
    )
    .get_matches();

    let status = if matches.is_present("REPL") {
        REPL
    } else if let Some(expression) = matches.value_of("expression") {
        EVAL(expression.to_string())
    } else if let Some(fname) = matches.value_of("INPUT") {
        let mut f = File::open(&fname)?;
        let mut source = String::new();
        f.read_to_string(&mut source)?;
        EVAL(source)
    } else {
        REPL
    };

    Ok(Some(status))
}
