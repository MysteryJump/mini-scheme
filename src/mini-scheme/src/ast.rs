use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevel {
    Expr(Expr),
    Define(Define),
    Load(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Const(Const),
    Id(String),
    Lambda(Arg, Body),
    Apply(Box<Expr>, Vec<Expr>),
    Quote(SExpr),
    Set(String, Box<Expr>),
    Let(Option<String>, Bindings, Body),
    LetStar(Bindings, Body),
    LetRec(Bindings, Body),
    If(Box<Expr>, Box<Expr>, Option<Box<Expr>>),
    Cond(Cond),
    And(Vec<Expr>),
    Or(Vec<Expr>),
    Begin(Vec<Expr>),
    Do(Do),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cond(pub Vec<(Expr, Vec<Expr>)>, pub Option<Vec<Expr>>);

#[derive(Debug, Clone, PartialEq)]
pub struct Do(
    pub Vec<(String, Expr, Expr)>,
    pub Box<Expr>,
    pub Vec<Expr>,
    pub Body,
);

#[derive(Debug, Clone, PartialEq)]
pub enum Define {
    Define(String, Expr),
    DefineList((String, Vec<String>, Option<String>), Body),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Const {
    Str(String),
    Bool(bool),
    Num(i64),
    Unit,
}

impl From<bool> for Const {
    fn from(s: bool) -> Self {
        Self::Bool(s)
    }
}

impl From<i64> for Const {
    fn from(s: i64) -> Self {
        Self::Num(s)
    }
}

impl From<String> for Const {
    fn from(s: String) -> Self {
        Self::Str(s)
    }
}

impl From<&str> for Const {
    fn from(s: &str) -> Self {
        Self::Str(s.to_string())
    }
}

impl From<()> for Const {
    fn from(_: ()) -> Self {
        Self::Unit
    }
}

impl Display for Const {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Const::Str(s) => write!(f, "{}", s),
            Const::Bool(true) => write!(f, "#t"),
            Const::Bool(false) => write!(f, "#f"),
            Const::Num(n) => write!(f, "{}", n),
            Const::Unit => write!(f, "()"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Body(pub Vec<Define>, pub Vec<Expr>);

#[derive(Debug, Clone, PartialEq)]
pub struct Bindings(pub Vec<(String, Expr)>);

#[derive(Debug, Clone, PartialEq)]
pub enum SExpr {
    Const(Const),
    Id(String),
    SExprs(Vec<SExpr>, Option<Box<SExpr>>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Arg {
    Id(String),
    IdList(Vec<String>, Option<String>),
}
