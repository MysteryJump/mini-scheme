#![feature(bool_to_option)]

use std::sync::{atomic::AtomicBool, Arc};

use either::Either;
use interpreter::{Env, ExecutionResult, Interpreter};
use parser::Parser;

use crate::ast::{Expr, SExpr, TopLevel};

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
    let mut interpreter = Interpreter::new(Arc::new(|x| println!("{}", x)), None);
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
    pub cancellation_token: Arc<AtomicBool>,
}

impl Repl {
    pub fn new() -> Self {
        let token = Arc::new(AtomicBool::new(false));
        Self {
            current_env: Env::new(Arc::new(|x| println!("{}", x)), Some(token.clone())),
            cancellation_token: token,
        }
    }

    pub fn execute_line(&mut self, line: &str) -> String {
        let lexed = lexer::lex(line);
        let parser = Parser::new(lexed);

        if let Some(pp) = parser.parse_toplevel() {
            let mut interpreter = Interpreter::with_env(self.current_env.clone());
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

pub struct Executer {
    interpreter: Interpreter,
}

impl Executer {
    pub fn new(lines: &str) -> Result<Self, String> {
        let lexed = lexer::lex(lines);
        let parser = Parser::new(lexed);
        let mut interpreter = Interpreter::new(Arc::new(|x| println!("{}", x)), None);
        while let Some(pp) = parser.parse_toplevel() {
            interpreter.execute_toplevel(pp);
        }
        let executor = Self { interpreter };
        Ok(executor)
    }

    pub fn execute_func(
        &mut self,
        name: &str,
        args: Vec<Box<dyn AsArg>>,
    ) -> Result<Either<ExecutionResult, Vec<ExecutionResult>>, String> {
        let r = self
            .interpreter
            .execute_toplevel(TopLevel::Expr(Expr::Apply(
                Box::new(Expr::Id(name.to_string())),
                args.iter().map(|x| x.to_const_expr()).collect(),
            )));
        match r {
            Either::Left(Ok(r)) => Ok(Either::Left(r)),
            Either::Left(Err(e)) => Err(e),
            Either::Right(r) => r
                .into_iter()
                .collect::<Result<Vec<_>, _>>()
                .map(Either::Right),
        }
    }
}

pub trait AsArg {
    fn to_const_expr(&self) -> Expr;
}

impl AsArg for &str {
    fn to_const_expr(&self) -> Expr {
        Expr::Const((*self).into())
    }
}

impl AsArg for i32 {
    fn to_const_expr(&self) -> Expr {
        Expr::Const((*self as i64).into())
    }
}

impl AsArg for i64 {
    fn to_const_expr(&self) -> Expr {
        Expr::Const((*self).into())
    }
}

impl AsArg for bool {
    fn to_const_expr(&self) -> Expr {
        Expr::Const((*self).into())
    }
}

impl AsArg for Vec<Box<dyn AsArg>> {
    fn to_const_expr(&self) -> Expr {
        let exprs = self
            .iter()
            .map(|x| x.to_const_expr())
            .map(|x| match x {
                Expr::Const(c) => SExpr::Const(c),
                Expr::Quote(SExpr::SExprs(s, _)) => SExpr::SExprs(s, None),
                _ => panic!(),
            })
            .collect::<Vec<_>>();
        Expr::Quote(SExpr::SExprs(exprs, None))
    }
}

#[test]
fn test_list_func() {
    let mut exec = Executer::new("(define (x y) y)").unwrap();
    let inner_inner: Vec<Box<dyn AsArg>> = vec![Box::new(23)];
    let list_inner: Vec<Box<dyn AsArg>> = vec![
        Box::new("30"),
        Box::new(34),
        Box::new(32),
        Box::new(inner_inner),
    ];
    let r = exec.execute_func("x", vec![Box::new(list_inner)]).unwrap();
    if let Either::Left(l) = r {
        assert_eq!(r#"("30" 34 32 (23))"#, l.to_string());
    }
}
