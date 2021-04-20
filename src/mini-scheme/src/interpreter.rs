use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    fmt::Display,
};

use crate::ast::{Arg, Body, Const, Define, Expr, TopLevel};

type EResult<'a> = Result<ExecutionResult<'a>, String>;

macro_rules! add_embfunc {
    ($map:ident,$($name:expr),*) => {
        $(
            $map.insert($name, ExecutionResult::EmbeddedFunc($name));
        )*
    };
}

#[derive(Debug, Default)]
pub struct Env<'a> {
    defineds: RefCell<HashMap<u32, HashMap<&'a str, ExecutionResult<'a>>>>,
    current_depth: Cell<u32>,
}

impl<'a> Env<'a> {
    pub fn new() -> Self {
        Self {
            defineds: {
                let mut map = HashMap::new();
                map.insert(0, Self::get_embedded_func_names());
                RefCell::new(map)
            },
            current_depth: Cell::new(0),
        }
    }

    pub fn add_define(&self, name: &'a str, result: ExecutionResult<'a>) {
        self.defineds
            .borrow_mut()
            .get_mut(&self.current_depth.get())
            .unwrap()
            .insert(name, result);
    }

    pub fn get_expr_by_def_name(&self, name: &'a str) -> Option<ExecutionResult<'a>> {
        let cdepth = self.current_depth.get();
        for i in 0..=cdepth {
            if self.defineds.borrow()[&(cdepth - i)].contains_key(name) {
                return Some(self.defineds.borrow_mut()[&(cdepth - i)][&name].clone());
            }
        }
        None
    }

    pub fn enter_block(&self) {
        let ndepth = self.current_depth.get() + 1;
        self.defineds.borrow_mut().insert(ndepth, HashMap::new());
        self.current_depth.set(ndepth);
    }

    pub fn exit_block(&self) {
        let cdepth = self.current_depth.get();
        self.defineds.borrow_mut().remove(&cdepth);
        self.current_depth.set(cdepth - 1);
    }

    pub fn update_entry(
        &self,
        name: &'a str,
        result: ExecutionResult<'a>,
    ) -> Result<ExecutionResult<'a>, ()> {
        let cdepth = self.current_depth.get();
        for i in 0..=cdepth {
            if self.defineds.borrow()[&(cdepth - i)].contains_key(name) {
                let s = self
                    .defineds
                    .borrow_mut()
                    .get_mut(&(cdepth - i))
                    .unwrap()
                    .insert(name, result);
                return if let Some(s) = s { Ok(s) } else { Err(()) };
            }
        }
        Err(())
    }

    fn get_embedded_func_names() -> HashMap<&'static str, ExecutionResult<'a>> {
        let mut map = HashMap::new();
        add_embfunc!(map, "number?", "+", "-", "*", "/", "=", "<", "<=", ">", ">=");
        add_embfunc!(
            map, "null?", "pair?", "list?", "symbol?", "car", "cdr", "cons", "list", "length",
            "memq", "last", "append", "set-car!", "set-cdr!"
        );
        add_embfunc!(map, "boolean?", "not");
        add_embfunc!(
            map,
            "string?",
            "string-append",
            "symbol->string",
            "string->symbol",
            "string->number",
            "number->string"
        );
        add_embfunc!(map, "procedure?");
        add_embfunc!(map, "eq?");
        add_embfunc!(map, "neq?");
        add_embfunc!(map, "equal?");
        add_embfunc!(map, "load");
        map
    }
}

#[derive(Debug)]
pub struct Interpreter<'a> {
    env: Env<'a>,
}

impl<'a> Interpreter<'a> {
    pub fn new() -> Self {
        Self { env: Env::new() }
    }

    pub fn execute_toplevel(&'a self, toplevel: TopLevel<'a>) -> EResult<'a> {
        match toplevel {
            TopLevel::Expr(e) => self.execute_expr(e),
            TopLevel::Define(def) => self.execute_define(def),
            TopLevel::Load(_) => todo!(),
        }
    }

    fn execute_expr(&'a self, expr: Expr<'a>) -> EResult<'a> {
        match expr {
            Expr::Const(c) => Ok(c.into()),
            Expr::Id(id) => self
                .env
                .get_expr_by_def_name(id)
                .ok_or_else(|| "Cannot find such name.".to_string()),
            Expr::Lambda(arg, body) => Ok(ExecutionResult::Func(arg, body)),
            Expr::Apply(func, arg_apply) => {
                let result_func = self.execute_expr(*func.clone())?;
                match result_func {
                    ExecutionResult::Func(arg_func, body) => match arg_func {
                        Arg::Id(id) => {
                            if arg_apply.len() != 1 {
                                Err("Args count is not match".to_string())
                            } else {
                                self.env.enter_block();
                                self.env
                                    .add_define(id, self.execute_expr(arg_apply[0].clone())?);
                                let result = self.execute_body(body);
                                self.env.exit_block();
                                result
                            }
                        }
                        Arg::IdList(ids, rest) => {
                            if rest.is_some() {
                                todo!()
                            }
                            if ids.len() != arg_apply.len() {
                                Err("Args count is not match".to_string())
                            } else {
                                self.env.enter_block();
                                for (key, expr) in ids.iter().zip(arg_apply) {
                                    self.env.add_define(key, self.execute_expr(expr.clone())?);
                                }
                                let result = self.execute_body(body);
                                self.env.exit_block();
                                result
                            }
                        }
                    },
                    ExecutionResult::EmbeddedFunc(name) => {
                        let evaleds = arg_apply
                            .iter()
                            .map(|x| self.execute_expr(x.clone()))
                            .collect::<Result<Vec<_>, _>>()?;
                        match name {
                            "+" => execute_number_binary_operation(NumOpKind::Add, &evaleds),
                            "-" => execute_number_binary_operation(NumOpKind::Sub, &evaleds),
                            "/" => execute_number_binary_operation(NumOpKind::Div, &evaleds),
                            "*" => execute_number_binary_operation(NumOpKind::Mul, &evaleds),
                            "=" => execute_number_binary_comparison(NumCompOpKind::Eq, &evaleds),
                            ">" => execute_number_binary_comparison(NumCompOpKind::Gt, &evaleds),
                            ">=" => execute_number_binary_comparison(NumCompOpKind::Gte, &evaleds),
                            "<" => execute_number_binary_comparison(NumCompOpKind::Lt, &evaleds),
                            "<=" => execute_number_binary_comparison(NumCompOpKind::Lte, &evaleds),
                            "string?" => execute_type_check(ExpectedTypes::String, &evaleds),
                            "boolean?" => execute_type_check(ExpectedTypes::Bool, &evaleds),
                            "number?" => execute_type_check(ExpectedTypes::Num, &evaleds),
                            "symbol?" => execute_type_check(ExpectedTypes::Symbol, &evaleds),
                            "procedure?" => execute_type_check(ExpectedTypes::Proc, &evaleds),
                            _ => todo!(),
                        }
                    }
                    _ => Err("Cannot apply to not-function".to_string()),
                }
            }
            Expr::Quote(_) => {
                todo!()
            }
            Expr::Set(name, expr) => self
                .env
                .update_entry(name, self.execute_expr(*expr.clone())?)
                .map_err(|_| "set expr to undefined name".to_string())
                .map(|_| ExecutionResult::Unit),
            Expr::Let(_, _, _) => {
                todo!()
            }
            Expr::LetStar(_, _) => {
                todo!()
            }
            Expr::LetRec(_, _) => {
                todo!()
            }
            Expr::If(cond, then, els) => {
                let cond = self.execute_expr(*cond.clone())?;
                if !matches!(
                    cond,
                    ExecutionResult::Bool(false) /*  | ExecutionResult::Unit */
                ) {
                    self.execute_expr(*then.clone())
                } else {
                    match els {
                        Some(els) => self.execute_expr(*els.clone()),
                        None => Ok(ExecutionResult::Unit),
                    }
                }
            }
            Expr::Cond(cond) => {
                for (con_expr, exprs) in cond.0 {
                    let result = self.execute_expr(con_expr)?;
                    if !matches!(result, ExecutionResult::Bool(false)) {
                        let result = exprs
                            .iter()
                            .map(|x| self.execute_expr(x.clone()))
                            .collect::<Result<Vec<_>, _>>()?;
                        return Ok(result.last().unwrap().clone());
                    }
                }
                if let Some(els) = cond.1 {
                    let result = els
                        .iter()
                        .map(|x| self.execute_expr(x.clone()))
                        .collect::<Result<Vec<_>, _>>()?;
                    Ok(result.last().unwrap().clone())
                } else {
                    Ok(ExecutionResult::Unit)
                }
            }
            Expr::And(exprs) => {
                let mut res = ExecutionResult::Unit;
                for expr in exprs {
                    res = self.execute_expr(expr)?;
                    if matches!(res, ExecutionResult::Bool(false)) {
                        break;
                    }
                }
                Ok(res)
            }
            Expr::Or(exprs) => {
                let mut res = ExecutionResult::Unit;
                for expr in exprs {
                    res = self.execute_expr(expr)?;
                    if !matches!(res, ExecutionResult::Bool(false)) {
                        break;
                    }
                }
                Ok(res)
            }
            Expr::Begin(begin) => {
                let last = begin
                    .iter()
                    .map(|x| self.execute_expr(x.clone()))
                    .collect::<Result<Vec<_>, _>>()?
                    .last()
                    .unwrap()
                    .clone();
                Ok(last)
            }
            Expr::Do(_) => {
                todo!()
            }
        }
    }

    fn execute_define(&'a self, define: Define<'a>) -> EResult<'a> {
        match define {
            Define::Define(name, expr) => {
                self.env.add_define(name, self.execute_expr(expr)?);
            }
            Define::DefineList((name, arg, arg_rest), body) => self.env.add_define(
                name,
                ExecutionResult::Func(Arg::IdList(arg, arg_rest), body),
            ),
        }
        Ok(ExecutionResult::Unit)
    }

    fn execute_body(&'a self, body: Body<'a>) -> EResult<'a> {
        body.0
            .iter()
            .map(|x| self.execute_define(x.clone()))
            .collect::<Result<Vec<_>, _>>()?;
        let results = body
            .1
            .iter()
            .map(|x| self.execute_expr(x.clone()))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(results.last().unwrap().clone())
    }
}

#[derive(Debug, Clone)]
pub enum ExecutionResult<'a> {
    Number(i64),
    String(&'a str),
    Bool(bool),
    #[allow(dead_code)]
    Symbol(String),
    Func(Arg<'a>, Body<'a>),
    #[allow(dead_code)]
    List,
    Unit,
    EmbeddedFunc(&'static str),
}

impl<'a> From<Const<'a>> for ExecutionResult<'a> {
    fn from(c: Const<'a>) -> Self {
        match c {
            Const::Str(s) => ExecutionResult::String(s),
            Const::Bool(b) => ExecutionResult::Bool(b),
            Const::Num(n) => ExecutionResult::Number(n),
            Const::Unit => ExecutionResult::Unit,
        }
    }
}

impl<'a> Display for ExecutionResult<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExecutionResult::Number(n) => write!(f, "{}", n),
            ExecutionResult::String(s) => write!(f, "{}", s),
            ExecutionResult::Bool(false) => write!(f, "#f"),
            ExecutionResult::Bool(true) => write!(f, "#t"),
            ExecutionResult::Symbol(_) => todo!(),
            ExecutionResult::Func(_, _) => write!(f, "#<procedure>"),
            ExecutionResult::Unit => write!(f, "()"),
            ExecutionResult::List => write!(f, "()"),
            ExecutionResult::EmbeddedFunc(_) => write!(f, "#<procedure>"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
enum NumOpKind {
    Add,
    Sub,
    Mul,
    Div,
}

impl Display for NumOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NumOpKind::Add => write!(f, "+"),
            NumOpKind::Sub => write!(f, "-"),
            NumOpKind::Mul => write!(f, "*"),
            NumOpKind::Div => write!(f, "/"),
        }
    }
}

fn execute_number_binary_operation<'a>(
    op_kind: NumOpKind,
    results: &[ExecutionResult<'a>],
) -> EResult<'a> {
    if results
        .iter()
        .any(|x| !matches!(x, ExecutionResult::Number(_)))
    {
        Err(format!("Cannot apply no-number value to `{}`", op_kind))
    } else {
        Ok(ExecutionResult::Number({
            let iter = results.iter().map(|x| {
                if let ExecutionResult::Number(x) = x {
                    x
                } else {
                    panic!()
                }
            });
            match op_kind {
                NumOpKind::Add => iter.sum(),
                NumOpKind::Sub => {
                    let mut iter = iter;
                    let first = *iter.next().unwrap();
                    iter.fold(first, |cur, next| cur - next)
                }
                NumOpKind::Mul => iter.product(),
                NumOpKind::Div => {
                    let mut iter = iter;
                    let mut first = if let Some(first) = iter.next() {
                        *first
                    } else {
                        return Err("number of argument needs 1 at least".to_string());
                    };
                    for item in iter {
                        if item == &0 {
                            return Err("division by zero".to_string());
                        }
                        first /= item;
                    }
                    first
                }
            }
        }))
    }
}

#[derive(Debug, Clone, Copy)]
enum NumCompOpKind {
    Lt,
    Lte,
    Gt,
    Gte,
    Eq,
}

impl Display for NumCompOpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NumCompOpKind::Lt => write!(f, "<"),
            NumCompOpKind::Lte => write!(f, "<="),
            NumCompOpKind::Gt => write!(f, ">"),
            NumCompOpKind::Gte => write!(f, ">="),
            NumCompOpKind::Eq => write!(f, "="),
        }
    }
}

fn execute_number_binary_comparison<'a>(
    op_kind: NumCompOpKind,
    results: &[ExecutionResult<'a>],
) -> EResult<'a> {
    if results.is_empty() {
        Err("number of arguments needs 1 at least".to_string())
    } else if results
        .iter()
        .any(|x| !matches!(x, ExecutionResult::Number(_)))
    {
        Err(format!("Cannot apply no-number value to `{}`", op_kind))
    } else {
        Ok(ExecutionResult::Bool({
            let mut iter = results.iter().map(|x| {
                if let ExecutionResult::Number(x) = x {
                    x
                } else {
                    panic!()
                }
            });
            let first = iter.next().unwrap();
            match op_kind {
                NumCompOpKind::Lt => iter.all(|next| (first < next)),
                NumCompOpKind::Lte => iter.all(|next| (first <= next)),
                NumCompOpKind::Gt => iter.all(|next| (first > next)),
                NumCompOpKind::Gte => iter.all(|next| (first >= next)),
                NumCompOpKind::Eq => iter.all(|next| (first == next)),
            }
        }))
    }
}

#[derive(Debug)]
enum ExpectedTypes {
    String,
    Num,
    Symbol,
    Bool,
    Proc,
    #[allow(dead_code)]
    List,
    #[allow(dead_code)]
    Pair,
    #[allow(dead_code)]
    Null,
}

fn execute_type_check<'a>(
    expected: ExpectedTypes,
    actually: &[ExecutionResult<'a>],
) -> EResult<'a> {
    if actually.len() != 1 {
        Err(("number of arguments needs `1`").to_string())
    } else {
        let f = &actually[0];
        Ok(ExecutionResult::Bool(match expected {
            ExpectedTypes::String => matches!(f, ExecutionResult::String(_)),
            ExpectedTypes::Num => matches!(f, ExecutionResult::Number(_)),
            ExpectedTypes::Symbol => matches!(f, ExecutionResult::Symbol(_)),
            ExpectedTypes::Bool => matches!(f, ExecutionResult::Bool(_)),
            ExpectedTypes::Proc => matches!(f, ExecutionResult::Func(_, _)),
            ExpectedTypes::List => todo!(),
            ExpectedTypes::Pair => todo!(),
            ExpectedTypes::Null => todo!(),
        }))
    }
}

#[test]
fn test_execute_number_binary_comp() {
    let result = execute_number_binary_comparison(
        NumCompOpKind::Lt,
        &vec![ExecutionResult::Number(5), ExecutionResult::Number(3)],
    )
    .unwrap();
    if let ExecutionResult::Bool(b) = result {
        assert_eq!(false, b);
    } else {
        panic!()
    }
}
