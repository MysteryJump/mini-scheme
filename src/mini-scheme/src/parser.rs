use if_chain::if_chain;
use std::{
    borrow::Borrow,
    cell::{Cell, RefCell},
};

use crate::{
    ast::{Arg, Bindings, Body, Define, Expr, SExpr, TopLevel},
    lexer::{Token, TokenKind},
};

use super::ast::Cond;

#[derive(Debug)]
pub struct TokenStack {
    ind: Cell<i32>,
    tokens: RefCell<Vec<Token>>,
}

impl TokenStack {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            ind: Cell::new(-1),
            tokens: RefCell::new(
                tokens
                    .into_iter()
                    .filter(|x| x.kind != TokenKind::Other)
                    .collect::<Vec<_>>(),
            ),
        }
    }

    pub fn next(&self) -> Option<Token> {
        if self.has_next() {
            self.ind.borrow().set(self.ind.get() + 1);
            Some((self.tokens.borrow_mut()[self.ind.get() as usize]).clone())
        } else {
            None
        }
    }

    pub fn eat(&self, kind: TokenKind) -> bool {
        if self.has_next() {
            self.ind.borrow().set(self.ind.get() + 1);
            let tk = &self.tokens.borrow_mut()[(self.ind.get()) as usize];
            tk.kind == kind
        } else {
            false
        }
    }

    pub fn lookahead(&self, n: i32) -> Option<Token> {
        if self.ind.get() + n < self.tokens.borrow().len() as i32 {
            Some(self.tokens.borrow()[(self.ind.get() + n) as usize].clone())
        } else {
            None
        }
    }

    #[inline]
    pub fn has_next(&self) -> bool {
        self.ind.get() + 1 < self.tokens.borrow().len() as i32
    }
}

type PResult<T> = Result<T, ParseError>;

pub struct Parser {
    tokens: TokenStack,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: TokenStack::new(tokens),
        }
    }

    pub fn parse_toplevel(&self) -> PResult<Option<TopLevel>> {
        match self.tokens.lookahead(1) {
            Some(s) => match s.kind {
                TokenKind::OpenParen => Ok(Some(match self.tokens.lookahead(2) {
                    Some(s) => match s.kind {
                        TokenKind::Ident(x) if x == "load" => {
                            self.tokens.next();
                            self.tokens.next();
                            let s = if let TokenKind::Str(id) = self.tokens.next().unwrap().kind {
                                id
                            } else {
                                self.handle_parse_error()?
                            };
                            self.eat_close()?;
                            TopLevel::Load(s)
                        }
                        TokenKind::Ident(x) if x == "define" => {
                            TopLevel::Define(self.parse_define()?)
                        }
                        TokenKind::Ident(x) if x == "define-and-run-actor" => {
                            let result = self.parse_define_actor()?;
                            TopLevel::DefineActor(result.0, result.1)
                        }
                        TokenKind::Ident(x) if x == "define-properties" => {
                            let result = self.parse_define_properties()?;
                            TopLevel::DefineProperties(result.0, result.1)
                        }
                        _ if self.is_next_first_of_expr() => TopLevel::Expr(self.parse_expr()?),
                        _ => self.handle_parse_error()?,
                    },
                    None => self.handle_parse_error()?,
                })),
                TokenKind::Quote
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Num(_)
                | TokenKind::Str(_)
                | TokenKind::Ident(_) => Ok(Some(TopLevel::Expr(self.parse_expr()?))),
                _ => self.handle_parse_error()?,
            },
            None => Ok(None),
        }
    }

    fn parse_expr(&self) -> PResult<Expr> {
        match self.tokens.next().unwrap().kind {
            TokenKind::OpenParen => match self.tokens.lookahead(1).unwrap().kind {
                TokenKind::Ident(x) if x == "lambda" => {
                    self.tokens.next();
                    let arg = if let TokenKind::Ident(_) | TokenKind::OpenParen =
                        self.tokens.lookahead(1).unwrap().kind
                    {
                        self.parse_arg()?
                    } else {
                        self.handle_parse_error()?
                    };
                    let body = if self.is_next_first_of_define() || self.is_next_first_of_expr() {
                        self.parse_body()?
                    } else {
                        self.handle_parse_error()?
                    };
                    self.eat_close()?;
                    Ok(Expr::Lambda(arg, body))
                }
                TokenKind::Ident(x) if x == "quote" => {
                    self.tokens.next();
                    let sexpr = if let TokenKind::Num(_)
                    | TokenKind::True
                    | TokenKind::False
                    | TokenKind::Str(_)
                    | TokenKind::OpenParen
                    | TokenKind::Ident(_) = self.tokens.lookahead(1).unwrap().kind
                    {
                        self.parse_sexpr()?
                    } else {
                        self.handle_parse_error()?
                    };
                    self.eat_close()?;
                    Ok(Expr::Quote(sexpr))
                }
                TokenKind::Ident(x) if x == "set!" => {
                    self.tokens.next();
                    let id = if let TokenKind::Ident(id) = self.tokens.next().unwrap().kind {
                        id
                    } else {
                        self.handle_parse_error()?
                    };
                    let expr = if self.is_next_first_of_expr() {
                        self.parse_expr()?
                    } else {
                        self.handle_parse_error()?
                    };

                    self.eat_close()?;
                    Ok(Expr::Set(id, Box::new(expr)))
                }
                TokenKind::Ident(x) if x == "let" => {
                    self.tokens.next();
                    let tp = match self.tokens.lookahead(1).unwrap().kind {
                        TokenKind::OpenParen => {
                            let binds = self.parse_bindings()?;
                            (None, binds)
                        }
                        TokenKind::Ident(id) => {
                            self.tokens.next();
                            let binds = self.parse_bindings()?;
                            (Some(id), binds)
                        }
                        _ => self.handle_parse_error()?,
                    };
                    let body = if self.is_next_first_of_define() | self.is_next_first_of_expr() {
                        self.parse_body()?
                    } else {
                        self.handle_parse_error()?
                    };

                    self.eat_close()?;
                    Ok(Expr::Let(tp.0, tp.1, body))
                }
                TokenKind::Ident(t) if t == "let*" || t == "letrec" => {
                    self.tokens.next();
                    let binds = if self.tokens.lookahead(1).unwrap().kind == TokenKind::OpenParen {
                        self.parse_bindings()?
                    } else {
                        self.handle_parse_error()?
                    };
                    let body = if self.is_next_first_of_define() | self.is_next_first_of_expr() {
                        self.parse_body()?
                    } else {
                        self.handle_parse_error()?
                    };

                    self.eat_close()?;
                    if t == "let*" {
                        Ok(Expr::LetStar(binds, body))
                    } else {
                        Ok(Expr::LetRec(binds, body))
                    }
                }
                TokenKind::Ident(t) if t == "if" => {
                    self.tokens.next();

                    let cond = if self.is_next_first_of_expr() {
                        self.parse_expr()?
                    } else {
                        self.handle_parse_error()?
                    };

                    let then = if self.is_next_first_of_expr() {
                        self.parse_expr()?
                    } else {
                        self.handle_parse_error()?
                    };

                    let els = if self.is_next_first_of_expr() {
                        Some(Box::new(self.parse_expr()?))
                    } else if self.tokens.lookahead(1).unwrap().kind == TokenKind::CloseParen {
                        None
                    } else {
                        self.handle_parse_error()?
                    };

                    self.eat_close()?;
                    Ok(Expr::If(Box::new(cond), Box::new(then), els))
                }
                TokenKind::Ident(x) if x == "cond" => {
                    self.tokens.next();
                    let mut els = None;
                    let mut conds = Vec::new();
                    while self.tokens.next().unwrap().kind == TokenKind::OpenParen {
                        match self.tokens.lookahead(1).unwrap().kind {
                            TokenKind::Ident(x) if x == "else" => {
                                self.tokens.next();
                                let mut exprs = Vec::new();
                                while self.is_next_first_of_expr() {
                                    exprs.push(self.parse_expr()?);
                                }
                                els = Some(exprs);
                                self.eat_close()?;
                                self.eat_close()?; // HACK: i don't know why, but it need
                                break;
                            }
                            _ => {}
                        }
                        if self.is_next_first_of_expr() {
                            let cond = self.parse_expr()?;
                            let mut exprs = Vec::new();
                            while self.is_next_first_of_expr() {
                                exprs.push(self.parse_expr()?);
                            }
                            conds.push((cond, exprs));
                            self.eat_close()?;
                        }
                    }

                    Ok(Expr::Cond(Cond(conds, els)))
                }
                TokenKind::Ident(t) if t == "and" || t == "or" || t == "begin" => {
                    self.tokens.next();
                    let mut exprs = Vec::new();
                    while self.is_next_first_of_expr() {
                        exprs.push(self.parse_expr()?);
                    }

                    self.eat_close()?;
                    match &t as &str {
                        "and" => Ok(Expr::And(exprs)),
                        "or" => Ok(Expr::Or(exprs)),
                        "begin" => Ok(Expr::Begin(exprs)),
                        _ => self.handle_parse_error()?,
                    }
                }
                TokenKind::Ident(d) if d == "do" => {
                    self.tokens.next();

                    self.tokens.eat(TokenKind::OpenParen).then_some(0).unwrap();
                    let mut itervar = Vec::new();
                    while TokenKind::OpenParen == self.tokens.lookahead(1).unwrap().kind {
                        self.tokens.next();
                        let id = if let TokenKind::Ident(id) = self.tokens.next().unwrap().kind {
                            id
                        } else {
                            self.handle_parse_error()?
                        };
                        let expr1 = if self.is_next_first_of_expr() {
                            self.parse_expr()?
                        } else {
                            self.handle_parse_error()?
                        };
                        let expr2 = if self.is_next_first_of_expr() {
                            self.parse_expr()?
                        } else {
                            self.handle_parse_error()?
                        };
                        itervar.push((id, expr1, expr2));
                        self.eat_close()?;
                    }
                    self.eat_close()?;

                    self.tokens.eat(TokenKind::OpenParen).then_some(0).unwrap();
                    let cond = if self.is_next_first_of_expr() {
                        self.parse_expr()?
                    } else {
                        self.handle_parse_error()?
                    };
                    let mut vals = Vec::new();
                    while self.is_next_first_of_expr() {
                        vals.push(self.parse_expr()?);
                    }
                    self.eat_close()?;

                    let body = if self.is_next_first_of_define() | self.is_next_first_of_expr() {
                        self.parse_body()?
                    } else {
                        self.handle_parse_error()?
                    };

                    self.eat_close()?;
                    Ok(Expr::Do(super::ast::Do(
                        itervar,
                        Box::new(cond),
                        vals,
                        body,
                    )))
                }
                TokenKind::CloseParen => {
                    self.eat_close()?;
                    Ok(Expr::Const(().into()))
                }
                _ if self.is_next_first_of_expr() => {
                    let t = self.parse_expr()?;
                    let mut vals = Vec::new();
                    while self.is_next_first_of_expr() {
                        vals.push(self.parse_expr()?);
                    }
                    self.eat_close()?;
                    Ok(Expr::Apply(Box::new(t), vals))
                }
                _ => self.handle_parse_error(),
            },
            TokenKind::Quote => {
                let sexpr = self.parse_sexpr()?;
                Ok(Expr::Quote(sexpr))
            }
            TokenKind::Ident(id) => Ok(Expr::Id(id)),
            TokenKind::Str(s) => Ok(Expr::Const(s.into())),
            TokenKind::True => Ok(Expr::Const(true.into())),
            TokenKind::False => Ok(Expr::Const(false.into())),
            TokenKind::Num(n) => Ok(Expr::Const(n.into())),
            _ => self.handle_parse_error(),
        }
    }

    // first: ( -> define(id)
    fn parse_define(&self) -> PResult<Define> {
        self.tokens.eat(TokenKind::OpenParen).then_some(0).unwrap();
        self.tokens
            .eat(TokenKind::Ident("define".to_string()))
            .then_some(0)
            .unwrap();
        match self.tokens.next().unwrap().kind {
            TokenKind::OpenParen => {
                let mut ids = Vec::new();
                let mut doted = false;
                loop {
                    match self.tokens.next().unwrap().kind {
                        TokenKind::CloseParen => break,
                        TokenKind::Dot => {
                            doted = true;
                            break;
                        }
                        TokenKind::Ident(id) => {
                            ids.push(id);
                        }
                        _ => return self.handle_parse_error(),
                    }
                }

                if ids.is_empty() {
                    return self.handle_parse_error();
                }
                let name = {
                    let n = ids[0].clone();
                    ids = ids[1..].to_vec();
                    n
                };
                let ids = (
                    name,
                    ids,
                    if doted {
                        if let TokenKind::Ident(id) = self.tokens.next().unwrap().kind {
                            self.tokens.eat(TokenKind::CloseParen).then_some(0).unwrap();
                            Some(id)
                        } else {
                            self.handle_parse_error()?
                        }
                    } else {
                        None
                    },
                );

                Ok(Define::DefineList(
                    ids,
                    if self.is_next_first_of_define() || self.is_next_first_of_expr() {
                        let b = self.parse_body()?;
                        self.eat_close()?;
                        b
                    } else {
                        self.handle_parse_error()?
                    },
                ))
            }
            TokenKind::Ident(id) if self.is_next_first_of_expr() => {
                let expr = self.parse_expr()?;
                self.eat_close()?;
                Ok(Define::Define(id, expr))
            }
            _ => self.handle_parse_error(),
        }
    }

    #[allow(clippy::type_complexity)]
    fn parse_define_actor(&self) -> PResult<((String, Vec<String>, Option<String>), Body)> {
        self.tokens.eat(TokenKind::OpenParen).then_some(0).unwrap();
        self.tokens
            .eat(TokenKind::Ident("define-and-run-actor".to_string()))
            .then_some(0)
            .unwrap();
        match self.tokens.next().unwrap().kind {
            TokenKind::OpenParen => {
                let mut ids = Vec::new();
                let mut doted = false;
                loop {
                    match self.tokens.next().unwrap().kind {
                        TokenKind::CloseParen => break,
                        TokenKind::Dot => {
                            doted = true;
                            break;
                        }
                        TokenKind::Ident(id) => {
                            ids.push(id);
                        }
                        _ => return self.handle_parse_error(),
                    }
                }

                if ids.is_empty() {
                    return self.handle_parse_error();
                }
                let name = {
                    let n = ids[0].clone();
                    ids = ids[1..].to_vec();
                    n
                };
                let ids = (
                    name,
                    ids,
                    if doted {
                        if let TokenKind::Ident(id) = self.tokens.next().unwrap().kind {
                            self.tokens.eat(TokenKind::CloseParen).then_some(0).unwrap();
                            Some(id)
                        } else {
                            self.handle_parse_error()?
                        }
                    } else {
                        None
                    },
                );
                let body = if self.is_next_first_of_define() || self.is_next_first_of_expr() {
                    let b = self.parse_body()?;
                    self.eat_close()?;
                    b
                } else {
                    self.handle_parse_error()?
                };

                Ok((ids, body))
            }
            _ => self.handle_parse_error(),
        }
    }

    fn parse_define_properties(&self) -> PResult<(Bindings, Bindings)> {
        self.tokens.eat(TokenKind::OpenParen).then_some(0).unwrap();
        self.tokens
            .eat(TokenKind::Ident("define-properties".to_string()))
            .then_some(0)
            .unwrap();
        let initials = self.parse_bindings()?;
        let flows = self.parse_bindings()?;
        self.eat_close()?;
        Ok((initials, flows))
    }

    // first: define or expr
    fn parse_body(&self) -> PResult<Body> {
        let mut defines = Vec::new();
        while self.is_next_first_of_define() {
            defines.push(self.parse_define()?);
        }
        let mut exprs = Vec::new();
        while self.is_next_first_of_expr() {
            exprs.push(self.parse_expr()?);
        }
        Ok(Body(defines, exprs))
    }

    // first: ( (-> Id , ))
    fn parse_bindings(&self) -> PResult<Bindings> {
        self.tokens.eat(TokenKind::OpenParen).then_some(0).unwrap();
        let mut binds = Vec::new();
        loop {
            match self.tokens.next().unwrap().kind {
                TokenKind::OpenParen => {
                    let id = if let TokenKind::Ident(id) = self.tokens.next().unwrap().kind {
                        id
                    } else {
                        return self.handle_parse_error();
                    };
                    let expr = if self.is_next_first_of_expr() {
                        self.parse_expr()?
                    } else {
                        return self.handle_parse_error();
                    };
                    binds.push((id, expr));
                    self.tokens.eat(TokenKind::CloseParen).then_some(0).unwrap();
                }
                TokenKind::CloseParen => {
                    break;
                }
                _ => return self.handle_parse_error(),
            }
        }
        Ok(Bindings(binds))
    }

    // first: (, Id
    fn parse_arg(&self) -> PResult<Arg> {
        Ok(match self.tokens.next().unwrap().kind {
            TokenKind::Ident(id) => Arg::Id(id),
            TokenKind::OpenParen => {
                let mut ids = Vec::new();
                while let TokenKind::Ident(id) = self.tokens.lookahead(1).unwrap().kind {
                    ids.push(id);
                    self.tokens.next();
                }
                let kind = self.tokens.next().unwrap();
                match kind.kind {
                    TokenKind::Dot => {
                        if let TokenKind::Ident(id) = self.tokens.next().unwrap().kind {
                            self.eat_close()?;
                            Arg::IdList(ids, Some(id))
                        } else {
                            return self.handle_parse_error();
                        }
                    }
                    TokenKind::CloseParen => Arg::IdList(ids, None),
                    _ => return self.handle_parse_error(),
                }
            }
            _ => return self.handle_parse_error(),
        })
    }

    // first: Num, Bool, String, (, Id
    fn parse_sexpr(&self) -> PResult<SExpr> {
        let next = self.tokens.next().unwrap();

        Ok(match next.kind {
            TokenKind::OpenParen => {
                if self.tokens.lookahead(1).unwrap().kind == TokenKind::CloseParen {
                    self.tokens.next();
                    SExpr::Const(().into())
                } else {
                    let mut has_dot = false;
                    let mut sexprs = Vec::new();
                    while let Some(t) = self.tokens.lookahead(1) {
                        let sexpr = match t.kind {
                            TokenKind::OpenParen
                            | TokenKind::Str(_)
                            | TokenKind::Ident(_)
                            | TokenKind::True
                            | TokenKind::False
                            | TokenKind::Num(_) => self.parse_sexpr()?,
                            TokenKind::CloseParen => {
                                self.eat_close()?;
                                break;
                            }
                            TokenKind::Dot => {
                                self.tokens.eat(TokenKind::Dot).then_some(0).unwrap();
                                has_dot = true;
                                break;
                            }
                            _ => {
                                return self.handle_parse_error();
                            }
                        };
                        sexprs.push(sexpr);
                    }

                    SExpr::SExprs(
                        sexprs,
                        if has_dot {
                            let expr = self.parse_sexpr()?;
                            self.eat_close()?;
                            Some(Box::new(expr))
                        } else {
                            None
                        },
                    )
                }
            }
            TokenKind::Str(s) => SExpr::Const(s.into()),
            TokenKind::Ident(id) => SExpr::Id(id),
            TokenKind::True => SExpr::Const(true.into()),
            TokenKind::False => SExpr::Const(false.into()),
            TokenKind::Num(n) => SExpr::Const(n.into()),
            _ => {
                return self.handle_parse_error();
            }
        })
    }

    fn is_next_first_of_expr(&self) -> bool {
        if self.is_next_first_of_define() {
            false
        } else if let Some(next) = self.tokens.lookahead(1) {
            matches!(
                next.kind,
                TokenKind::OpenParen
                    | TokenKind::Str(_)
                    | TokenKind::Num(_)
                    | TokenKind::True
                    | TokenKind::False
                    | TokenKind::Ident(_)
                    | TokenKind::Quote
            )
        } else {
            false
        }
    }

    fn is_next_first_of_define(&self) -> bool {
        if_chain! {
            if let Some(s) = self.tokens.lookahead(1);
            if s.kind == TokenKind::OpenParen;

            if let Some(s) = self.tokens.lookahead(2);
            if let TokenKind::Ident(def) = s.kind;
            then {
                def == "define"
            } else {
                false
            }
        }
    }

    fn eat_close(&self) -> Result<(), ParseError> {
        if self.tokens.eat(TokenKind::CloseParen) {
            Ok(())
        } else {
            self.handle_parse_error()
        }
    }

    fn handle_parse_error<T>(&self) -> Result<T, ParseError> {
        if let Some(t) = self.tokens.lookahead(1) {
            Err(ParseError::UnexpectedToken(t))
        } else {
            Err(ParseError::UnexpectedEof)
        }
    }
}

#[derive(Debug, thiserror::Error)]
pub enum ParseError {
    #[error("Unexpected token `{:?}` near ln:{} col:{}", .0.kind, .0.span.ln, .0.span.col)]
    UnexpectedToken(Token),
    #[error("Unexpected EoF")]
    UnexpectedEof,
}

#[cfg(test)]
mod tests {
    use crate::{ast::Do, lexer};

    use super::*;

    fn get_toplevel_ast(src: &'static str) -> TopLevel {
        let tokens = lexer::lex(src);
        let parser = Parser::new(tokens);
        parser.parse_toplevel().unwrap().unwrap()
    }

    fn get_expr(src: &'static str) -> Expr {
        let tokens = lexer::lex(src);
        let parser = Parser::new(tokens);
        parser.parse_expr().unwrap()
    }

    fn get_body(src: &'static str) -> Body {
        let tokens = lexer::lex(src);
        let parser = Parser::new(tokens);
        parser.parse_body().unwrap()
    }

    fn get_bindings(src: &'static str) -> Bindings {
        let tokens = lexer::lex(src);
        let parser = Parser::new(tokens);
        parser.parse_bindings().unwrap()
    }

    fn get_sexpr(src: &'static str) -> SExpr {
        let tokens = lexer::lex(src);
        let parser = Parser::new(tokens);
        parser.parse_sexpr().unwrap()
    }

    fn get_arg(src: &'static str) -> Arg {
        let tokens = lexer::lex(src);
        let parser = Parser::new(tokens);
        parser.parse_arg().unwrap()
    }

    #[test]
    fn test_parse_load() {
        let toplevel = get_toplevel_ast(r#"(load "test.scm")"#);
        assert_eq!(TopLevel::Load("test.scm".to_string()), toplevel);
    }

    #[test]
    fn test_parse_define() {
        let tl = get_toplevel_ast("(define a 3)");
        assert_eq!(
            TopLevel::Define(Define::Define("a".to_string(), Expr::Const(3.into()))),
            tl
        );
        let tl = get_toplevel_ast("(define (a b c) 3)");
        assert_eq!(
            TopLevel::Define(Define::DefineList(
                (
                    "a".to_string(),
                    vec!["b".to_string(), "c".to_string()],
                    None
                ),
                Body(Vec::new(), vec![Expr::Const(3.into())])
            ),),
            tl
        );
        let tl = get_toplevel_ast("(define (a b . c) 3)");
        assert_eq!(
            TopLevel::Define(Define::DefineList(
                (
                    "a".to_string(),
                    vec!["b".to_string()],
                    Some("c".to_string())
                ),
                Body(Vec::new(), vec![Expr::Const(3.into())])
            )),
            tl
        )
    }

    #[test]
    fn test_const() {
        let c = get_expr("3");
        assert_eq!(Expr::Const(3.into()), c);
        let c = get_expr("#t");
        assert_eq!(Expr::Const(true.into()), c);
        let c = get_expr("-3");
        assert_eq!(Expr::Const((-3).into()), c);
        let c = get_expr("\"src\"");
        assert_eq!(Expr::Const("src".into()), c);
        let c = get_expr("()");
        assert_eq!(Expr::Const(().into()), c);
    }

    #[test]
    fn test_id() {
        let id = get_expr("<src>");
        assert_eq!(Expr::Id("<src>".to_string()), id);
        let id = get_expr("src");
        assert_eq!(Expr::Id("src".to_string()), id);
        let id = get_expr("-3.3");
        assert_eq!(Expr::Id("-3.3".to_string()), id);
    }

    #[test]
    fn test_body() {
        let body = get_body("(define a 30) (define b 50) (+ 50 30)");
        assert_eq!(
            Body(
                vec![
                    Define::Define("a".to_string(), Expr::Const(30.into())),
                    Define::Define("b".to_string(), Expr::Const(50.into()))
                ],
                vec![Expr::Apply(
                    Box::new(Expr::Id("+".to_string())),
                    vec![Expr::Const(50.into()), Expr::Const(30.into())]
                )]
            ),
            body
        );
    }

    #[test]
    fn test_bindings() {
        let b = get_bindings("((a 30) (b 50))");
        assert_eq!(
            Bindings(vec![
                ("a".to_string(), Expr::Const(30.into())),
                ("b".to_string(), Expr::Const(50.into()))
            ]),
            b
        );
    }

    #[test]
    fn test_sexpr() {
        let s = get_sexpr("(3 () 3 . (4 . 3))");
        assert_eq!(
            SExpr::SExprs(
                vec![
                    SExpr::Const(3.into()),
                    SExpr::Const(().into()), // not SExpr::SExprs(Vec::new(), None)
                    SExpr::Const(3.into())
                ],
                Some(Box::new(SExpr::SExprs(
                    vec![SExpr::Const(4.into())],
                    Some(Box::new(SExpr::Const(3.into())))
                )))
            ),
            s
        );
    }

    #[test]
    fn test_arg() {
        let a = get_arg("id");
        assert_eq!(Arg::Id("id".to_string()), a);
        let a = get_arg("(id id . id)");
        assert_eq!(
            Arg::IdList(
                vec!["id".to_string(), "id".to_string()],
                Some("id".to_string())
            ),
            a
        );
    }

    #[test]
    fn test_lambda() {
        let l = get_expr("(lambda x 3)");
        assert_eq!(
            Expr::Lambda(
                Arg::Id("x".to_string()),
                Body(Vec::new(), vec![Expr::Const(3.into())])
            ),
            l
        );
    }

    #[test]
    fn test_apply() {
        let a = get_expr("(+ 3 3)");
        assert_eq!(
            Expr::Apply(
                Box::new(Expr::Id("+".to_string())),
                vec![Expr::Const(3.into()), Expr::Const(3.into())]
            ),
            a
        );
    }

    #[test]
    fn test_set() {
        let s = get_expr("(set! a 0)");
        assert_eq!(
            Expr::Set("a".to_string(), Box::new(Expr::Const(0.into()))),
            s
        );
    }

    #[test]
    fn test_quote() {
        let q = get_expr("(quote (3))");
        assert_eq!(
            Expr::Quote(SExpr::SExprs(vec![SExpr::Const(3.into())], None)),
            q
        );
        let q = get_expr("'(3)");
        assert_eq!(
            Expr::Quote(SExpr::SExprs(vec![SExpr::Const(3.into())], None)),
            q
        );
    }

    #[test]
    fn test_let() {
        let l = get_expr("(let id () 3)");
        assert_eq!(
            Expr::Let(
                Some("id".to_string()),
                Bindings(Vec::new()),
                Body(Vec::new(), vec![Expr::Const(3.into())])
            ),
            l
        );
        let l = get_expr("(let () 3)");
        assert_eq!(
            Expr::Let(
                None,
                Bindings(Vec::new()),
                Body(Vec::new(), vec![Expr::Const(3.into())])
            ),
            l
        );
        let l = get_expr("(let* ((o (+ 3 4))) 3)");
        assert_eq!(
            Expr::LetStar(
                Bindings(vec![(
                    "o".to_string(),
                    Expr::Apply(
                        Box::new(Expr::Id("+".to_string())),
                        vec![Expr::Const(3.into()), Expr::Const(4.into())]
                    )
                )]),
                Body(Vec::new(), vec![Expr::Const(3.into())])
            ),
            l
        );
        let l = get_expr("(letrec () 3)");
        assert_eq!(
            Expr::LetRec(
                Bindings(Vec::new()),
                Body(Vec::new(), vec![Expr::Const(3.into())])
            ),
            l
        );
    }

    #[test]
    fn test_if() {
        let i = get_expr("(if #t #f #f)");
        assert_eq!(
            Expr::If(
                Box::new(Expr::Const(true.into())),
                Box::new(Expr::Const(false.into())),
                Some(Box::new(Expr::Const(false.into())))
            ),
            i
        );
        let i = get_expr("(if #f #f)");
        assert_eq!(
            Expr::If(
                Box::new(Expr::Const(false.into())),
                Box::new(Expr::Const(false.into())),
                None
            ),
            i
        );
    }

    #[test]
    fn test_cond() {
        let c = get_expr("(cond (#t 3 3) (#f 3))");
        assert_eq!(
            Expr::Cond(Cond(
                vec![
                    (
                        Expr::Const(true.into()),
                        vec![Expr::Const(3.into()), Expr::Const(3.into())]
                    ),
                    (Expr::Const(false.into()), vec![Expr::Const(3.into())])
                ],
                None
            )),
            c
        );
        let c = get_expr("(cond (#t 3 3) (else 3))");
        assert_eq!(
            Expr::Cond(Cond(
                vec![(
                    Expr::Const(true.into()),
                    vec![Expr::Const(3.into()), Expr::Const(3.into())]
                ),],
                Some(vec![Expr::Const(3.into())])
            )),
            c
        );
    }

    #[test]
    fn test_and_or_begin() {
        let a = get_expr("(and #t #f)");
        assert_eq!(
            Expr::And(vec![Expr::Const(true.into()), Expr::Const(false.into())]),
            a
        );
    }

    #[test]
    fn test_do() {
        let d = get_expr("(do ((id 3 (+ id 7))) (#t id) 3)");
        assert_eq!(
            Expr::Do(Do(
                vec![(
                    "id".to_string(),
                    Expr::Const(3.into()),
                    Expr::Apply(
                        Box::new(Expr::Id("+".to_string())),
                        vec![Expr::Id("id".to_string()), Expr::Const(7.into())],
                    )
                )],
                Box::new(Expr::Const(true.into())),
                vec![Expr::Id("id".to_string())],
                Body(Vec::new(), vec![Expr::Const(3.into())])
            )),
            d
        );
    }
}
