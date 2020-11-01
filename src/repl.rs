use crate::eval::*;
use crate::parse::*;
use crate::utils::*;
use rustyline::{config::Config, error::ReadlineError, highlight::*, validate::*, Editor};
use rustyline::{Cmd, KeyPress};
use rustyline_derive::{Completer, Helper, Hinter};
use std::borrow::Cow::{self, Borrowed, Owned};
use std::io;

pub fn repl() -> io::Result<()> {
    let conf = Config::builder().auto_add_history(true).build();
    let mut rl = Editor::with_config(conf);
    rl.set_helper(Some(MyHelper::default()));
    rl.bind_sequence(KeyPress::Ctrl('\\'), Cmd::Insert(1, String::from("λ")));
    let mut env = prelude_env();

    for readline in rl.iter("> ") {
        match readline {
            Ok(line) => {
                let line = line.trim();
                if line.is_empty() {
                    continue;
                }
                match parse_repl_input(&line).and_then(|ast| interp(&ast, &mut env)) {
                    Ok(RetValue::Unit) => {}
                    Ok(ret) => {
                        println!("{}", ret);
                    }
                    Err(e) => {
                        println!("Error: \n{}", e);
                    }
                }
            }
            Err(ReadlineError::Interrupted) => {
                println!("Exit");
                break;
            }
            Err(ReadlineError::Eof) => {
                println!("Exit");
                break;
            }
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
    Ok(())
}

#[derive(Helper, Hinter, Completer, Default)]
struct MyHelper {
    completer: (),
    highlighter: MatchingBracketHighlighter,
    hinter: (),
    validator: MatchingBracketValidator,
}

impl Highlighter for MyHelper {
    fn highlight_prompt<'b, 's: 'b, 'p: 'b>(
        &'s self,
        prompt: &'p str,
        _default: bool,
    ) -> Cow<'b, str> {
        Borrowed(prompt)
    }

    fn highlight_hint<'h>(&self, hint: &'h str) -> Cow<'h, str> {
        Owned("\x1b[1m".to_owned() + hint + "\x1b[m")
    }

    fn highlight<'l>(&self, line: &'l str, pos: usize) -> Cow<'l, str> {
        self.highlighter.highlight(line, pos)
    }

    fn highlight_char(&self, line: &str, pos: usize) -> bool {
        self.highlighter.highlight_char(line, pos)
    }
}

impl Validator for MyHelper {
    fn validate(&self, ctx: &mut ValidationContext) -> rustyline::Result<ValidationResult> {
        self.validator.validate(ctx)
    }

    fn validate_while_typing(&self) -> bool {
        self.validator.validate_while_typing()
    }
}
