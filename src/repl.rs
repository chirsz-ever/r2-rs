use super::r2;
use rustyline::{
    config::Config,
    error::ReadlineError,
    highlight::{Highlighter, MatchingBracketHighlighter},
    Editor,
};
use rustyline_derive::{Completer, Helper, Hinter};
use std::borrow::Cow::{self, Borrowed, Owned};
use std::io;

pub fn repl() -> io::Result<()> {
    let conf = Config::builder().auto_add_history(true).build();
    let mut rl = Editor::with_config(conf);
    rl.set_helper(Some(MyHelper::new()));

    for readline in rl.iter("> ") {
        match readline {
            Ok(line) => {
                if line.is_empty() {
                    continue;
                }
                let r = r2(&line).unwrap();
                println!("{}", r);
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

#[derive(Helper, Hinter, Completer)]
struct MyHelper {
    completer: (),
    highlighter: MatchingBracketHighlighter,
    hinter: (),
    colored_prompt: String,
}

impl MyHelper {
    fn new() -> Self {
        MyHelper {
            completer: (),
            highlighter: MatchingBracketHighlighter::new(),
            hinter: (),
            colored_prompt: "".to_owned(),
        }
    }
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
