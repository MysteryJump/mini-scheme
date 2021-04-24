#![feature(bool_to_option)]

use std::sync::Arc;

use either::Either;
use interpreter::{Env, Interpreter};
use parser::Parser;

mod ast;
mod interpreter;
mod lexer;
mod parser;

pub fn execute(source: &str) -> Vec<Result<String, String>> {
    let lexed = lexer::lex(source);
    // println!(
    //     "{:#?}",
    //     lexed
    //         .iter()
    //         .map(|x| x.kind.clone())
    //         .filter(|x| !(x == &lexer::TokenKind::Other))
    //         .collect::<Vec<_>>()
    // );
    let mut results = Vec::new();
    let parser = Parser::new(lexed);
    let interpreter = Interpreter::new(Arc::new(|x| println!("{}", x)));
    while let Some(pp) = parser.parse_toplevel() {
        match interpreter.execute_toplevel(pp) {
            Either::Left(result) => {
                results.push(result.map(|x| x.to_string()));
            }
            Either::Right(r) => {
                results.append(&mut r.iter().map(|x| x.clone().map(|y| y.to_string())).collect());
            }
        }
    }
    results
}

#[derive(Clone)]
pub struct Repl {
    current_env: Env,
}

impl Repl {
    pub fn new() -> Self {
        Self {
            current_env: Env::new(Arc::new(|x| println!("{}", x))),
        }
    }

    pub fn execute_line(&mut self, line: &str) -> String {
        let lexed = lexer::lex(line);
        let parser = Parser::new(lexed);

        if let Some(pp) = parser.parse_toplevel() {
            let interpreter = Interpreter::with_env(self.current_env.clone());
            let str = match interpreter.execute_toplevel(pp) {
                Either::Left(result) => match result {
                    Ok(r) => r.to_string(),
                    Err(e) => e,
                },
                Either::Right(r) => {
                    let mut s = String::new();
                    for item in r {
                        match item {
                            Ok(ss) => {
                                if !matches!(ss, interpreter::ExecutionResult::Unit) {
                                    s.push_str(&ss.to_string());
                                    s.push('\n')
                                }
                            }
                            Err(e) => {
                                s.push_str(&e);
                                s.push('\n')
                            }
                        }
                    }
                    s
                }
            };
            self.current_env = interpreter.get_env();
            str
        } else {
            "".to_string()
        }
    }

    pub fn get_current_env(&self) -> &Env {
        &self.current_env
    }
}

impl Default for Repl {
    fn default() -> Self {
        Self::new()
    }
}
