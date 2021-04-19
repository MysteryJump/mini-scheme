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

pub struct TokenStack<'a> {
    ind: Cell<i32>,
    tokens: RefCell<Vec<&'a Token<'a>>>,
}

impl<'a> TokenStack<'a> {
    pub fn new(tokens: &'a [Token<'a>]) -> Self {
        Self {
            tokens: std::cell::RefCell::new(
                tokens
                    .iter()
                    .filter(|x| x.kind != TokenKind::Other)
                    .collect::<Vec<_>>(),
            ),
            ind: std::cell::Cell::new(-1),
        }
    }

    pub fn next(&self) -> Option<&'a Token<'a>> {
        if self.has_next() {
            self.ind.borrow().set(self.ind.get() + 1);
            Some(&(self.tokens.borrow_mut()[self.ind.get() as usize]))
        } else {
            None
        }
    }

    pub fn eat(&self, kind: TokenKind<'a>) -> bool {
        if self.has_next() {
            self.ind.borrow().set(self.ind.get() + 1);
            let tk = &self.tokens.borrow_mut()[(self.ind.get()) as usize];
            tk.kind == kind
        } else {
            false
        }
    }

    pub fn lookahead(&self, n: i32) -> Option<&'a Token<'a>> {
        if self.ind.get() + n < self.tokens.borrow().len() as i32 {
            Some(&self.tokens.borrow()[(self.ind.get() + n) as usize])
        } else {
            None
        }
    }

    #[inline]
    pub fn has_next(&self) -> bool {
        self.ind.get() + 1 < self.tokens.borrow().len() as i32
    }
}

pub struct Parser<'a> {
    tokens: TokenStack<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token<'a>]) -> Self {
        Self {
            tokens: TokenStack::new(tokens),
        }
    }

    pub fn parse_toplevel(&self) -> Option<TopLevel> {
        match self.tokens.lookahead(1) {
            Some(s) => match s.kind {
                TokenKind::OpenParen => Some(match self.tokens.lookahead(2) {
                    Some(s) => match s.kind {
                        TokenKind::Ident("load") => {
                            self.tokens.next();
                            self.tokens.next();
                            let s = if let TokenKind::Ident(id) = self.tokens.next().unwrap().kind {
                                id
                            } else {
                                panic!()
                            };
                            self.eat_close().unwrap();
                            TopLevel::Load(s)
                        }
                        TokenKind::Ident("define") => TopLevel::Define(self.parse_define()),
                        _ if self.is_next_first_of_expr() => TopLevel::Expr(self.parse_expr()),
                        _ => panic!(),
                    },
                    None => {
                        panic!()
                    }
                }),
                TokenKind::Quote
                | TokenKind::True
                | TokenKind::False
                | TokenKind::Num(_)
                | TokenKind::Str(_)
                | TokenKind::Ident(_) => Some(TopLevel::Expr(self.parse_expr())),
                _ => {
                    panic!()
                }
            },
            None => None,
        }
    }

    fn parse_expr(&self) -> Expr {
        match self.tokens.next().unwrap().kind {
            TokenKind::OpenParen => match self.tokens.lookahead(1).unwrap().kind {
                TokenKind::Ident("lambda") => {
                    self.tokens.next();
                    let arg = if let TokenKind::Ident(_) | TokenKind::OpenParen =
                        self.tokens.lookahead(1).unwrap().kind
                    {
                        self.parse_arg()
                    } else {
                        panic!()
                    };
                    let body = if self.is_next_first_of_define() || self.is_next_first_of_expr() {
                        self.parse_body()
                    } else {
                        panic!()
                    };
                    self.eat_close().unwrap();
                    Expr::Lambda(arg, body)
                }
                TokenKind::Ident("quote") => {
                    self.tokens.next();
                    let sexpr = if let TokenKind::Num(_)
                    | TokenKind::True
                    | TokenKind::False
                    | TokenKind::Str(_)
                    | TokenKind::OpenParen
                    | TokenKind::Ident(_) = self.tokens.lookahead(1).unwrap().kind
                    {
                        self.parse_sexpr()
                    } else {
                        panic!()
                    };
                    self.eat_close().unwrap();
                    Expr::Quote(sexpr)
                }
                TokenKind::Ident("set!") => {
                    self.tokens.next();
                    let id = if let TokenKind::Ident(id) = self.tokens.next().unwrap().kind {
                        id
                    } else {
                        panic!()
                    };
                    let expr = if self.is_next_first_of_expr() {
                        self.parse_expr()
                    } else {
                        panic!()
                    };

                    self.eat_close().unwrap();
                    Expr::Set(id, Box::new(expr))
                }
                TokenKind::Ident("let") => {
                    self.tokens.next();
                    let tp = match self.tokens.lookahead(1).unwrap().kind {
                        TokenKind::OpenParen => {
                            let binds = self.parse_bindings();
                            (None, binds)
                        }
                        TokenKind::Ident(id) => {
                            self.tokens.next();
                            let binds = self.parse_bindings();
                            (Some(id), binds)
                        }
                        _ => {
                            panic!()
                        }
                    };
                    let body = if self.is_next_first_of_define() | self.is_next_first_of_expr() {
                        self.parse_body()
                    } else {
                        panic!()
                    };

                    self.eat_close().unwrap();
                    Expr::Let(tp.0, tp.1, body)
                }
                TokenKind::Ident(t @ ("let*" | "letrec")) => {
                    self.tokens.next();
                    let binds = if self.tokens.lookahead(1).unwrap().kind == TokenKind::OpenParen {
                        self.parse_bindings()
                    } else {
                        panic!()
                    };
                    let body = if self.is_next_first_of_define() | self.is_next_first_of_expr() {
                        self.parse_body()
                    } else {
                        panic!()
                    };

                    self.eat_close().unwrap();
                    if t == "let*" {
                        Expr::LetStar(binds, body)
                    } else {
                        Expr::LetRec(binds, body)
                    }
                }
                TokenKind::Ident("if") => {
                    self.tokens.next();

                    let cond = if self.is_next_first_of_expr() {
                        self.parse_expr()
                    } else {
                        panic!()
                    };

                    let then = if self.is_next_first_of_expr() {
                        self.parse_expr()
                    } else {
                        panic!()
                    };

                    let els = if self.is_next_first_of_expr() {
                        Some(Box::new(self.parse_expr()))
                    } else if self.tokens.lookahead(1).unwrap().kind == TokenKind::CloseParen {
                        None
                    } else {
                        panic!()
                    };

                    self.eat_close().unwrap();
                    Expr::If(Box::new(cond), Box::new(then), els)
                }
                TokenKind::Ident("cond") => {
                    self.tokens.next();
                    let mut els = None;
                    let mut conds = Vec::new();
                    while self.tokens.next().unwrap().kind == TokenKind::OpenParen {
                        if let TokenKind::Ident("else") = self.tokens.lookahead(1).unwrap().kind {
                            self.tokens.next();
                            let mut exprs = Vec::new();
                            while self.is_next_first_of_expr() {
                                exprs.push(self.parse_expr());
                            }
                            els = Some(exprs);
                            self.eat_close().unwrap();
                            break;
                        }
                        if self.is_next_first_of_expr() {
                            let cond = self.parse_expr();
                            let mut exprs = Vec::new();
                            while self.is_next_first_of_expr() {
                                exprs.push(self.parse_expr());
                            }
                            conds.push((cond, exprs));
                            self.eat_close().unwrap();
                        }
                    }

                    self.eat_close().unwrap();
                    Expr::Cond(Cond(conds, els))
                }
                TokenKind::Ident(t @ ("and" | "or" | "begin")) => {
                    self.tokens.next();
                    let mut exprs = Vec::new();
                    while self.is_next_first_of_expr() {
                        exprs.push(self.parse_expr());
                    }

                    self.eat_close().unwrap();
                    match t {
                        "and" => Expr::And(exprs),
                        "or" => Expr::Or(exprs),
                        "begin" => Expr::Begin(exprs),
                        _ => panic!(),
                    }
                }
                TokenKind::Ident("do") => {
                    self.tokens.next();

                    self.tokens.eat(TokenKind::OpenParen).then_some(0).unwrap();
                    let mut itervar = Vec::new();
                    while TokenKind::OpenParen == self.tokens.lookahead(1).unwrap().kind {
                        self.tokens.next();
                        let id = if let TokenKind::Ident(id) = self.tokens.next().unwrap().kind {
                            id
                        } else {
                            panic!()
                        };
                        let expr1 = if self.is_next_first_of_expr() {
                            self.parse_expr()
                        } else {
                            panic!()
                        };
                        let expr2 = if self.is_next_first_of_expr() {
                            self.parse_expr()
                        } else {
                            panic!()
                        };
                        itervar.push((id, expr1, expr2));
                        self.eat_close().unwrap();
                    }
                    self.eat_close().unwrap();

                    self.tokens.eat(TokenKind::OpenParen).then_some(0).unwrap();
                    let cond = if self.is_next_first_of_expr() {
                        self.parse_expr()
                    } else {
                        panic!()
                    };
                    let mut vals = Vec::new();
                    while self.is_next_first_of_expr() {
                        vals.push(self.parse_expr());
                    }
                    self.eat_close().unwrap();

                    let body = if self.is_next_first_of_define() | self.is_next_first_of_expr() {
                        self.parse_body()
                    } else {
                        panic!()
                    };

                    self.eat_close().unwrap();
                    Expr::Do(super::ast::Do(itervar, Box::new(cond), vals, body))
                }
                TokenKind::CloseParen => {
                    self.tokens.next();
                    self.eat_close().unwrap();
                    Expr::Const(().into())
                }
                _ if self.is_next_first_of_expr() => {
                    let t = self.parse_expr();
                    let mut vals = Vec::new();
                    while self.is_next_first_of_expr() {
                        vals.push(self.parse_expr());
                    }
                    self.eat_close().unwrap();
                    Expr::Apply(Box::new(t), vals)
                }
                _ => panic!(),
            },
            TokenKind::Quote => {
                let sexpr = self.parse_sexpr();
                println!("{:?}", self.tokens.lookahead(1));
                Expr::Quote(sexpr)
            }
            TokenKind::Ident(id) => Expr::Id(id),
            TokenKind::Str(s) => Expr::Const(s.into()),
            TokenKind::True => Expr::Const(true.into()),
            TokenKind::False => Expr::Const(false.into()),
            TokenKind::Num(n) => Expr::Const(n.into()),
            _ => panic!(),
        }
    }

    // first: ( -> define(id)
    fn parse_define(&self) -> Define {
        self.tokens.eat(TokenKind::OpenParen).then_some(0).unwrap();
        self.tokens
            .eat(TokenKind::Ident("define"))
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
                        _ => panic!(),
                    }
                }

                if ids.is_empty() {
                    panic!()
                }
                let name = {
                    let n = ids[0];
                    ids = (&ids[1..]).to_vec();
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
                            panic!()
                        }
                    } else {
                        None
                    },
                );

                Define::DefineList(
                    ids,
                    if self.is_next_first_of_define() || self.is_next_first_of_expr() {
                        let b = self.parse_body();
                        self.eat_close().unwrap();
                        b
                    } else {
                        panic!()
                    },
                )
            }
            TokenKind::Ident(id) if self.is_next_first_of_expr() => {
                let expr = self.parse_expr();
                self.eat_close().unwrap();
                Define::Define(id, expr)
            }
            _ => {
                panic!();
            }
        }
    }

    // first: define or expr
    fn parse_body(&self) -> Body {
        let mut defines = Vec::new();
        while self.is_next_first_of_define() {
            defines.push(self.parse_define());
        }
        let mut exprs = Vec::new();
        while self.is_next_first_of_expr() {
            exprs.push(self.parse_expr());
        }
        Body(defines, exprs)
    }

    // first: ( (-> Id , ))
    fn parse_bindings(&self) -> Bindings {
        self.tokens.eat(TokenKind::OpenParen);
        let mut binds = Vec::new();
        loop {
            match self.tokens.next().unwrap().kind {
                TokenKind::OpenParen => {
                    let id = if let TokenKind::Ident(id) = self.tokens.next().unwrap().kind {
                        id
                    } else {
                        panic!()
                    };
                    let expr = if self.is_next_first_of_expr() {
                        self.parse_expr()
                    } else {
                        panic!();
                    };
                    binds.push((id, expr));
                    self.tokens.eat(TokenKind::CloseParen).then_some(0).unwrap();
                }
                TokenKind::CloseParen => {
                    break;
                }
                _ => panic!(),
            }
        }
        Bindings(binds)
    }

    // first: (, Id
    fn parse_arg(&self) -> Arg {
        match self.tokens.next().unwrap().kind {
            TokenKind::Ident(id) => Arg::Id(id),
            TokenKind::OpenParen => {
                let mut ids = Vec::new();
                while let TokenKind::Ident(id) = self.tokens.next().unwrap().kind {
                    ids.push(id);
                }
                match self.tokens.next().unwrap().kind {
                    TokenKind::Dot => {
                        if let TokenKind::Ident(id) = self.tokens.next().unwrap().kind {
                            Arg::IdList(ids, Some(id))
                        } else {
                            panic!()
                        }
                    }
                    TokenKind::CloseParen => Arg::IdList(ids, None),
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }

    // first: Num, Bool, String, (, Id
    fn parse_sexpr(&self) -> SExpr {
        let next = self.tokens.next().unwrap().clone();

        match next.kind {
            TokenKind::OpenParen => {
                if self.tokens.lookahead(1).unwrap().kind == TokenKind::CloseParen {
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
                            | TokenKind::Num(_) => self.parse_sexpr(),
                            TokenKind::CloseParen => {
                                self.eat_close().unwrap();
                                break;
                            }
                            TokenKind::Dot => {
                                self.tokens.eat(TokenKind::Dot).then_some(0).unwrap();
                                has_dot = true;
                                break;
                            }
                            _ => {
                                panic!()
                            }
                        };
                        sexprs.push(sexpr);
                    }

                    SExpr::SExprs(
                        sexprs,
                        if has_dot {
                            let expr = self.parse_sexpr();
                            self.eat_close().unwrap();
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
                panic!()
            }
        }
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

    fn eat_close(&self) -> Result<(), ()> {
        self.tokens
            .eat(TokenKind::CloseParen)
            .then_some(())
            .ok_or(())
    }
}
