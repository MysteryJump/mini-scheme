#[derive(Debug)]
pub enum TopLevel {
    Expr(Expr),
    Define(Define),
}

#[derive(Debug)]
pub enum Expr {}

#[derive(Debug)]
pub enum Define {}

#[derive(Debug)]
pub enum Const {}

#[derive(Debug)]
pub enum Body {}

#[derive(Debug)]
pub enum Bindings {}

#[derive(Debug)]
pub enum SExpr {}

#[derive(Debug)]
pub enum Arg {}

pub enum NonTerminal {}
