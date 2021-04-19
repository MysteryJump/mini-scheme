use std::fmt::Display;

#[derive(Debug, Clone)]
pub enum TopLevel<'a> {
    Expr(Expr<'a>),
    Define(Define<'a>),
    Load(&'a str),
}

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    Const(Const<'a>),
    Id(&'a str),
    Lambda(Arg<'a>, Body<'a>),
    Apply(Box<Expr<'a>>, Vec<Expr<'a>>),
    Quote(SExpr<'a>),
    Set(&'a str, Box<Expr<'a>>),
    Let(Option<&'a str>, Bindings<'a>, Body<'a>),
    LetStar(Bindings<'a>, Body<'a>),
    LetRec(Bindings<'a>, Body<'a>),
    If(Box<Expr<'a>>, Box<Expr<'a>>, Option<Box<Expr<'a>>>),
    Cond(Cond<'a>),
    And(Vec<Expr<'a>>),
    Or(Vec<Expr<'a>>),
    Begin(Vec<Expr<'a>>),
    Do(Do<'a>),
}

#[derive(Debug, Clone)]
pub struct Cond<'a>(
    pub Vec<(Expr<'a>, Vec<Expr<'a>>)>,
    pub Option<Vec<Expr<'a>>>,
);

#[derive(Debug, Clone)]
pub struct Do<'a>(
    pub Vec<(&'a str, Expr<'a>, Expr<'a>)>,
    pub Box<Expr<'a>>,
    pub Vec<Expr<'a>>,
    pub Body<'a>,
);

#[derive(Debug, Clone)]
pub enum Define<'a> {
    Define(&'a str, Expr<'a>),
    DefineList((&'a str, Vec<&'a str>, Option<&'a str>), Body<'a>),
}

#[derive(Debug, Clone)]
pub enum Const<'a> {
    Str(&'a str),
    Bool(bool),
    Num(i64),
    Unit,
}

impl<'a> From<bool> for Const<'a> {
    fn from(s: bool) -> Self {
        Self::Bool(s)
    }
}

impl<'a> From<i64> for Const<'a> {
    fn from(s: i64) -> Self {
        Self::Num(s)
    }
}

impl<'a> From<&'a str> for Const<'a> {
    fn from(s: &'a str) -> Self {
        Self::Str(s)
    }
}

impl<'a> From<()> for Const<'a> {
    fn from(_: ()) -> Self {
        Self::Unit
    }
}

impl<'a> Display for Const<'a> {
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

#[derive(Debug, Clone)]
pub struct Body<'a>(pub Vec<Define<'a>>, pub Vec<Expr<'a>>);

#[derive(Debug, Clone)]
pub struct Bindings<'a>(pub Vec<(&'a str, Expr<'a>)>);

#[derive(Debug, Clone)]
pub enum SExpr<'a> {
    Const(Const<'a>),
    Id(&'a str),
    SExprs(Vec<SExpr<'a>>, Option<Box<SExpr<'a>>>),
}

#[derive(Debug, Clone)]
pub enum Arg<'a> {
    Id(&'a str),
    IdList(Vec<&'a str>, Option<&'a str>),
}
