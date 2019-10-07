#![allow(dead_code)]

#[macro_use]
extern crate clap;

mod eval;
mod parse;
mod repl;
mod utils;

use std::fs::File;
use std::io::{self, prelude::*};
use utils::{r2, err_info};

#[derive(Debug)]
enum Status {
    REPL,
    EVAL(String),
}

use Status::*;

fn main() -> io::Result<()> {
    match deal_arg()? {
        REPL => repl::repl()?,
        EVAL(exp) => {
            match r2(&exp) {
                Ok(ret) => {
                    println!("{}", ret);
                }
                Err(e) => {
                    println!("Error: \n{}", err_info(&exp, e));
                }
            }
            
        }
    }

    Ok(())
}

fn deal_arg() -> io::Result<Status> {
    let matches = clap_app!(r2c =>
        (author: "Chris Ever. <chirsz@foxmail.com>")
        (about: "R2 Interpreter Implemented with Rust")
        (usage: "r2 <source file>\n    r2 -i\n    r2 -c <expression>")
        (@arg INPUT: conflicts_with[CMD] "Sets the input file to use")
        (@arg REPL: -i conflicts_with[INPUT CMD] "Interactive REPL mode")
        (@arg expression: -c +takes_value "Expression passed in as string")
    )
    .get_matches();

    let status = if matches.is_present("REPL") {
        REPL
    } else if let Some(expression) = matches.value_of("expression") {
        EVAL(expression.to_string())
    } else {
        let fname = matches.value_of("INPUT").unwrap();
        let mut f = File::open(&fname)?;
        let mut source = String::new();
        f.read_to_string(&mut source)?;
        EVAL(source)
    };

    Ok(status)
}

