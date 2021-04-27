use std::{
    cell::{Cell, RefCell},
    collections::{HashMap, HashSet},
    fmt::Display,
    sync::Arc,
};

use either::Either;
use uuid::Uuid;

use crate::{
    ast::{Arg, Body, Const, Define, Expr, SExpr, TopLevel},
    lexer,
};

type EResult = Result<ExecutionResult, String>;

macro_rules! add_embfunc {
    ($map:ident,$($name:expr),*) => {
        $(
            $map.insert($name.to_string(), ExecutionResult::EmbeddedFunc($name));
        )*
    };
}

#[derive(Clone)]
pub struct Env {
    defineds: RefCell<HashMap<u32, HashMap<String, ExecutionResult>>>,
    overwritten_builtins: RefCell<HashSet<String>>,
    builtins: HashMap<String, ExecutionResult>,
    current_depth: Cell<u32>,
    str_consts_pairs: RefCell<HashMap<String, u128>>,
    logger: Arc<dyn Fn(String) + Sync + Send>,
}

impl std::fmt::Debug for Env {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        f.debug_struct("Env")
            .field("defineds", &self.defineds)
            .field("current_depth", &self.current_depth)
            .field("str_consts_pairs", &self.str_consts_pairs)
            .field("overwritten_builtins", &self.overwritten_builtins)
            .finish()
    }
}

impl Env {
    pub fn new(logger: Arc<dyn Fn(String) + Sync + Send>) -> Self {
        let builtins = Self::get_embedded_func_names();
        Self {
            defineds: {
                let mut map = HashMap::new();
                map.insert(0, builtins.clone());
                RefCell::new(map)
            },
            current_depth: Cell::new(0),
            str_consts_pairs: RefCell::new(HashMap::new()),
            logger,
            overwritten_builtins: RefCell::new(HashSet::new()),
            builtins,
        }
    }

    pub fn add_define(&self, name: String, result: ExecutionResult) {
        if self.builtins.contains_key(&name) {
            self.overwritten_builtins.borrow_mut().insert(name.clone());
        }
        self.defineds
            .borrow_mut()
            .get_mut(&self.current_depth.get())
            .unwrap()
            .insert(name, result);
    }

    pub fn add_defines(&self, pairs: Vec<(String, ExecutionResult)>) {
        for (name, result) in pairs {
            self.add_define(name, result);
        }
    }

    pub fn get_expr_by_def_name(&self, name: String) -> Option<ExecutionResult> {
        if self.builtins.contains_key(&name) && !self.overwritten_builtins.borrow().contains(&name)
        {
            return Some(self.builtins[&name].clone());
        }
        let cdepth = self.current_depth.get();
        for i in 0..=cdepth {
            if self.defineds.borrow()[&(cdepth - i)].contains_key(&name) {
                let r = self.defineds.borrow()[&(cdepth - i)][&name].clone();
                return Some(r);
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
        name: String,
        result: ExecutionResult,
    ) -> Result<ExecutionResult, ()> {
        let cdepth = self.current_depth.get();
        for i in 0..=cdepth {
            if self.defineds.borrow()[&(cdepth - i)].contains_key(&name) {
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

    fn get_embedded_func_names() -> HashMap<String, ExecutionResult> {
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
        add_embfunc!(map, "display");
        map
    }

    pub fn get_const_str_addr(&self, name: String) -> u128 {
        if self.str_consts_pairs.borrow().contains_key(&name) {
            self.str_consts_pairs.borrow()[&name]
        } else {
            let addr = Uuid::new_v4().as_u128();
            self.str_consts_pairs.borrow_mut().insert(name, addr);
            addr
        }
    }
}

#[derive(Debug)]
pub struct Interpreter {
    env: Env,
}

impl Interpreter {
    pub fn new(logger: Arc<dyn Fn(String) + Sync + Send>) -> Self {
        Self {
            env: Env::new(logger),
        }
    }

    pub fn with_env(env: Env) -> Self {
        Self { env }
    }

    pub fn get_env(self) -> Env {
        self.env
    }

    pub fn execute_toplevel(&self, toplevel: TopLevel) -> Either<EResult, Vec<EResult>> {
        match toplevel {
            TopLevel::Expr(e) => Either::Left(self.execute_expr(e)),
            TopLevel::Define(def) => Either::Left(self.execute_define(def)),
            TopLevel::Load(path) => {
                #[cfg(all(target_arch = "wasm32", target_os = "unknown"))]
                return Either::Left(Err("Unsupported function usage in this platform."));

                Either::Right({
                    let src = match std::fs::read_to_string(path)
                        .map_err(|_| "Cannot read file.".to_string())
                    {
                        Ok(o) => o,
                        Err(e) => return Either::Left(Err(e)),
                    };
                    let lexer = lexer::lex(&src);
                    let parser = crate::parser::Parser::new(lexer);
                    let mut results = Vec::new();
                    while let Some(pp) = parser.parse_toplevel() {
                        match self.execute_toplevel(pp) {
                            Either::Left(l) => {
                                results.push(l);
                            }
                            Either::Right(mut r) => {
                                results.append(&mut r);
                            }
                        }
                    }
                    results
                })
            }
        }
    }

    fn execute_expr(&self, mut expr: Expr) -> EResult {
        let mut tail_depth = 0;
        loop {
            let r = match &expr {
                Expr::Const(c) => Ok(c.clone().into_with_env(&self.env)),
                Expr::Id(id) => self
                    .env
                    .get_expr_by_def_name(id.clone())
                    .ok_or_else(|| "Cannot find such name.".to_string()),
                Expr::Lambda(arg, body) => Ok(ExecutionResult::Func(
                    arg.clone(),
                    body.clone(),
                    Uuid::new_v4().as_u128(),
                )),
                Expr::Apply(func, arg_apply) => {
                    let result_func = self.execute_expr(*func.clone())?;
                    match result_func {
                        ExecutionResult::Func(arg_func, body, _) => match arg_func {
                            Arg::Id(id) => {
                                if arg_apply.len() != 1 {
                                    Err("Args count is not match".to_string())
                                } else {
                                    self.env.enter_block();
                                    self.env
                                        .add_define(id, self.execute_expr(arg_apply[0].clone())?);
                                    let result = self.execute_body_with_tail(body)?;
                                    expr = result;
                                    tail_depth += 1;
                                    continue;
                                }
                            }
                            Arg::IdList(ids, rest) => {
                                if let Some(rest) = rest {
                                    if ids.len() > arg_apply.len() {
                                        Err("Args count is fewer".to_string())
                                    } else {
                                        let ids_len = ids.len();
                                        let ids = if ids.len() < arg_apply.len() {
                                            let mut ids = ids;
                                            ids.append(
                                                &mut std::iter::repeat(rest.clone())
                                                    .take(arg_apply.len() - ids.len())
                                                    .collect(),
                                            );
                                            ids
                                        } else {
                                            ids
                                        };
                                        let executed_results =
                                            ids.iter().zip(arg_apply).map(|(name, expr)| {
                                                (name, self.execute_expr(expr.clone()))
                                            });

                                        let args = executed_results
                                            .clone()
                                            .take(ids.len())
                                            .collect::<Vec<_>>();

                                        let mut rests = Vec::new();
                                        for (_, r) in executed_results.skip(ids_len) {
                                            rests.push(r?);
                                        }
                                        let list = ExecutionResult::List((rests, None).into());

                                        self.env.enter_block();
                                        for (name, result) in args {
                                            self.env.add_define(name.to_string(), result?);
                                        }

                                        self.env.add_define(rest, list);

                                        let result = self.execute_body_with_tail(body)?;
                                        expr = result;
                                        tail_depth += 1;
                                        continue;
                                    }
                                } else if ids.len() != arg_apply.len() {
                                    Err("Args count is not match".to_string())
                                } else {
                                    self.env.enter_block();
                                    let mut binds = Vec::new();
                                    for (key, result) in ids
                                        .iter()
                                        .zip(arg_apply)
                                        .map(|(name, expr)| (name, self.execute_expr(expr.clone())))
                                    {
                                        binds.push((key.to_string(), result?));
                                    }
                                    self.env.add_defines(binds);
                                    let result = self.execute_body_with_tail(body)?;
                                    expr = result;
                                    tail_depth += 1;
                                    continue;
                                }
                            }
                        },
                        ExecutionResult::EmbeddedFunc(name) => {
                            let evaleds = arg_apply
                                .iter()
                                .map(|x| self.execute_expr(x.clone()))
                                .collect::<Result<Vec<_>, _>>()?;
                            match &name as &str {
                                "+" => execute_number_binary_operation(NumOpKind::Add, &evaleds),
                                "-" => execute_number_binary_operation(NumOpKind::Sub, &evaleds),
                                "/" => execute_number_binary_operation(NumOpKind::Div, &evaleds),
                                "*" => execute_number_binary_operation(NumOpKind::Mul, &evaleds),
                                "=" => {
                                    execute_number_binary_comparison(NumCompOpKind::Eq, &evaleds)
                                }
                                ">" => {
                                    execute_number_binary_comparison(NumCompOpKind::Gt, &evaleds)
                                }
                                ">=" => {
                                    execute_number_binary_comparison(NumCompOpKind::Gte, &evaleds)
                                }
                                "<" => {
                                    execute_number_binary_comparison(NumCompOpKind::Lt, &evaleds)
                                }
                                "<=" => {
                                    execute_number_binary_comparison(NumCompOpKind::Lte, &evaleds)
                                }
                                "string?" => execute_type_check(Types::String, &evaleds),
                                "boolean?" => execute_type_check(Types::Bool, &evaleds),
                                "number?" => execute_type_check(Types::Num, &evaleds),
                                "symbol?" => execute_type_check(Types::Symbol, &evaleds),
                                "procedure?" => execute_type_check(Types::Proc, &evaleds),
                                "not" => execute_not(&evaleds),
                                "string->number" => {
                                    execute_conversion(ConvType::Str, ConvType::Num, &evaleds)
                                }
                                "number->string" => {
                                    execute_conversion(ConvType::Num, ConvType::Str, &evaleds)
                                }
                                "string->symbol" => {
                                    execute_conversion(ConvType::Str, ConvType::Sym, &evaleds)
                                }
                                "symbol->string" => {
                                    execute_conversion(ConvType::Sym, ConvType::Str, &evaleds)
                                }
                                "string-append" => execute_string_append(&evaleds),
                                "null?" => execute_type_check(Types::Null, &evaleds),
                                "pair?" => execute_type_check(Types::Pair, &evaleds),
                                "list?" => execute_type_check(Types::List, &evaleds),
                                "car" => execute_list_operation(ListOperationKind::Car, &evaleds),
                                "cdr" => execute_list_operation(ListOperationKind::Cdr, &evaleds),
                                "cons" => execute_list_operation(ListOperationKind::Cons, &evaleds),
                                "list" => execute_list_operation(ListOperationKind::List, &evaleds),
                                "length" => {
                                    execute_list_operation(ListOperationKind::Length, &evaleds)
                                }
                                "last" => execute_list_operation(ListOperationKind::Last, &evaleds),
                                "append" => {
                                    execute_list_operation(ListOperationKind::Append, &evaleds)
                                }
                                "set-car!" => {
                                    let result = execute_list_operation(
                                        ListOperationKind::SetCar,
                                        &evaleds,
                                    )?;
                                    if let Expr::Id(id) = arg_apply[0].clone() {
                                        self.env
                                            .update_entry(id, result)
                                            .map_err(|_| "Cannot find such name.".to_string())?;
                                    }
                                    Ok(ExecutionResult::Unit)
                                }
                                "set-cdr!" => {
                                    let result = execute_list_operation(
                                        ListOperationKind::SetCdr,
                                        &evaleds,
                                    )?;
                                    if let Expr::Id(id) = arg_apply[0].clone() {
                                        self.env
                                            .update_entry(id, result)
                                            .map_err(|_| "Cannot find such name.".to_string())?;
                                    }
                                    Ok(ExecutionResult::Unit)
                                }
                                "eq?" => execute_comp_operation(EqCompKind::Eq, &evaleds),
                                "neq?" => execute_comp_operation(EqCompKind::Neq, &evaleds),
                                "equal?" => execute_comp_operation(EqCompKind::Equal, &evaleds),
                                "memq" => execute_list_operation(ListOperationKind::Memq, &evaleds),
                                "display" => {
                                    if evaleds.len() != 1 {
                                        Err(("a number of argument needs 1").to_string())
                                    } else {
                                        (*self.env.logger.clone())(evaleds[0].to_string());
                                        Ok(ExecutionResult::Unit)
                                    }
                                }
                                _ => todo!(),
                            }
                        }
                        _ => Err("Cannot apply to not-function".to_string()),
                    }
                }
                Expr::Quote(se) => Ok(match se {
                    SExpr::Const(c) => {
                        if matches!(c, Const::Unit) {
                            ExecutionResult::List(List::Nil)
                        } else {
                            c.clone().into_with_env(&self.env)
                        }
                    }
                    SExpr::Id(sym) => ExecutionResult::Symbol(sym.clone()),
                    SExpr::SExprs(m, rest) => {
                        let results = m
                            .iter()
                            .map(|x| self.execute_expr(Expr::Quote(x.clone())))
                            .collect::<Result<Vec<_>, _>>()?;
                        let rest = if let Some(rest) = rest {
                            Some(self.execute_expr(Expr::Quote(*rest.clone()))?)
                        } else {
                            None
                        };
                        ExecutionResult::List((results, rest).into())
                    }
                }),
                Expr::Set(name, expr) => self
                    .env
                    .update_entry(name.clone(), self.execute_expr(*expr.clone())?)
                    .map_err(|_| "set expr to undefined name".to_string())
                    .map(|_| ExecutionResult::Unit),
                Expr::Let(name, binds, body) => {
                    let mut calced_bindeds = Vec::new();
                    for (name, expr) in &binds.0 {
                        calced_bindeds.push((name.clone(), self.execute_expr(expr.clone())?));
                    }
                    self.env.enter_block();
                    self.env.add_defines(calced_bindeds);
                    let result = if let Some(name) = name {
                        let arg_ls = binds.0.iter().map(|x| x.0.clone()).collect();
                        let func = ExecutionResult::Func(
                            Arg::IdList(arg_ls, None),
                            body.clone(),
                            Uuid::new_v4().as_u128(),
                        );
                        self.env.add_define(name.clone(), func);

                        self.execute_body_with_tail(body.clone())?
                    } else {
                        self.execute_body_with_tail(body.clone())?
                    };
                    tail_depth += 1;
                    expr = result;
                    continue;
                }
                Expr::LetStar(binds, body) => {
                    let envs_depth = binds.0.len();
                    for (name, expr) in &binds.0 {
                        self.env
                            .add_define(name.to_string(), self.execute_expr(expr.clone())?);
                        self.env.enter_block();
                    }
                    let result = self.execute_body_with_tail(body.clone())?;
                    tail_depth += envs_depth;
                    expr = result;
                    continue;
                }
                Expr::LetRec(binds, body) => {
                    let names = binds.0.iter().map(|x| x.0.clone()).collect::<Vec<_>>();
                    if names.len() != names.iter().collect::<HashSet<_>>().len() {
                        Err("Cannot use same variable in bindings of letrec".to_string())
                    } else {
                        self.env.enter_block();
                        let undefineds = names
                            .iter()
                            .map(|x| (x.clone(), ExecutionResult::Undefined))
                            .collect();
                        self.env.add_defines(undefineds);

                        let mut calced_bindeds = Vec::new();
                        for (name, expr) in &binds.0 {
                            calced_bindeds.push((name.clone(), self.execute_expr(expr.clone())?));
                        }
                        for (name, result) in calced_bindeds {
                            self.env.update_entry(name, result).unwrap();
                        }
                        let result = self.execute_body_with_tail(body.clone())?;
                        tail_depth += 1;
                        expr = result;
                        continue;
                    }
                }
                Expr::If(cond, then, els) => {
                    let cond = self.execute_expr(*cond.clone())?;
                    if !matches!(cond, ExecutionResult::Bool(false)) {
                        expr = *then.clone();
                        continue;
                    } else {
                        match els {
                            Some(els) => {
                                expr = *els.clone();
                                continue;
                            }
                            None => Ok(ExecutionResult::Unit),
                        }
                    }
                }
                Expr::Cond(cond) => {
                    for (con_expr, exprs) in cond.0.clone() {
                        let result = self.execute_expr(con_expr)?;
                        if !matches!(result, ExecutionResult::Bool(false)) {
                            let result = exprs
                                .iter()
                                .map(|x| self.execute_expr(x.clone()))
                                .collect::<Result<Vec<_>, _>>()?;
                            return Ok(result.last().unwrap().clone());
                        }
                    }
                    if let Some(els) = cond.1.clone() {
                        let body = Body(Vec::new(), els.clone());
                        let result = self.execute_body_with_tail(body)?;
                        expr = result;
                        continue;
                    } else {
                        Ok(ExecutionResult::Unit)
                    }
                }
                Expr::And(exprs) => {
                    let mut res = ExecutionResult::Unit;
                    if !exprs.is_empty() {
                        let last_expr = exprs.last().unwrap().clone();
                        for expr in exprs.iter().take(exprs.len() - 1) {
                            res = self.execute_expr(expr.clone())?;
                            if matches!(res, ExecutionResult::Bool(false)) {
                                break;
                            }
                        }
                        if matches!(res, ExecutionResult::Bool(false)) {
                            Ok(res)
                        } else {
                            expr = last_expr;
                            continue;
                        }
                    } else {
                        Ok(ExecutionResult::Bool(true))
                    }
                }
                Expr::Or(exprs) => {
                    let mut res = ExecutionResult::Unit;
                    if !exprs.is_empty() {
                        let last_expr = exprs.last().unwrap().clone();
                        for exp in exprs.iter().take(exprs.len() - 1) {
                            res = self.execute_expr(exp.clone())?;
                            if !matches!(res, ExecutionResult::Bool(false)) {
                                break;
                            }
                        }
                        if !matches!(res, ExecutionResult::Bool(false)) {
                            Ok(res)
                        } else {
                            expr = last_expr;
                            continue;
                        }
                    } else {
                        Ok(ExecutionResult::Bool(false))
                    }
                }
                Expr::Begin(begin) => {
                    let body = Body(Vec::new(), begin.clone());
                    let result = self.execute_body_with_tail(body)?;
                    expr = result;
                    continue;
                }
                Expr::Do(d) => {
                    self.env.enter_block();
                    let mut map = HashMap::new();
                    let mut binds = Vec::new();
                    for (name, init, step) in &d.0 {
                        binds.push((name.to_string(), self.execute_expr(init.clone())?));
                        map.insert(name, step.clone());
                    }
                    self.env.add_defines(binds);
                    let mut is_first = true;
                    let tail_expr;
                    loop {
                        if !is_first {
                            let mut updates = Vec::new();
                            for (name, step) in &map {
                                updates.push((name.to_string(), self.execute_expr(step.clone())?));
                            }
                            for (name, result) in updates {
                                self.env
                                    .update_entry(name, result)
                                    .map_err(|_| "Cannot find such a identifier")?;
                            }
                        }
                        let r = self.execute_expr(*d.1.clone())?;
                        if !matches!(r, ExecutionResult::Bool(false)) {
                            // d.2 is tail sequence
                            let body = Body(Vec::new(), d.2.clone());
                            let tail = self.execute_body_with_tail(body)?;
                            tail_expr = tail;

                            break;
                        } else {
                            self.execute_body(d.3.clone())?;
                        }
                        is_first = false;
                    }
                    tail_depth += 1;
                    expr = tail_expr;
                    continue;
                }
            };
            for _ in 0..tail_depth {
                self.env.exit_block();
            }
            return r;
        }
    }

    fn execute_define(&self, define: Define) -> EResult {
        match define {
            Define::Define(name, expr) => {
                self.env.add_define(name, self.execute_expr(expr)?);
            }
            Define::DefineList((name, arg, arg_rest), body) => self.env.add_define(
                name,
                ExecutionResult::Func(Arg::IdList(arg, arg_rest), body, Uuid::new_v4().as_u128()),
            ),
        }
        Ok(ExecutionResult::Unit)
    }

    fn execute_body(&self, body: Body) -> EResult {
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

    fn execute_body_with_tail(&self, body: Body) -> Result<Expr, String> {
        body.0
            .iter()
            .map(|x| self.execute_define(x.clone()))
            .collect::<Result<Vec<_>, _>>()?;
        let expr_len = body.1.len();
        let last = body.1.last().unwrap().clone();
        body.1
            .iter()
            .take(expr_len - 1)
            .map(|x| self.execute_expr(x.clone()))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(last)
    }
}

#[derive(Debug, Clone)]
pub enum ExecutionResult {
    Number(i64),
    String(String, u128),
    Bool(bool),
    Symbol(String),
    Func(Arg, Body, u128),
    List(List),
    Unit,
    EmbeddedFunc(&'static str),
    Undefined,
}

impl ExecutionResult {
    /// do deep compare
    fn equal(&self, other: &Self) -> bool {
        match self {
            ExecutionResult::Number(n) => matches!(other, ExecutionResult::Number(nn) if n == nn),
            ExecutionResult::String(s, _) => {
                matches!(other, ExecutionResult::String(ss,_) if s == ss)
            }
            ExecutionResult::Bool(b) => matches!(other, ExecutionResult::Bool(bb) if b == bb),
            ExecutionResult::Symbol(s) => matches!(other, ExecutionResult::Symbol(ss) if s == ss),
            ExecutionResult::Func(_, _, a) => {
                matches!(other, ExecutionResult::Func(_,_,aa) if a == aa)
            }
            ExecutionResult::List(l) => matches!(other, ExecutionResult::List(ll) if l.equal(ll)),
            ExecutionResult::Unit => matches!(other, ExecutionResult::Unit),
            ExecutionResult::EmbeddedFunc(e) => {
                matches!(other, ExecutionResult::EmbeddedFunc(ee) if e == ee)
            }
            ExecutionResult::Undefined => false,
        }
    }
    /// do shallow compare
    fn eq(&self, other: &Self) -> bool {
        match self {
            ExecutionResult::Number(nx) => {
                matches!(other, ExecutionResult::Number(n) if nx == n)
            }
            ExecutionResult::String(_, addr) => {
                matches!(other, ExecutionResult::String(_, addr2) if addr == addr2)
            }
            ExecutionResult::Bool(b) => {
                matches!(other, ExecutionResult::Bool(b2) if b == b2)
            }
            ExecutionResult::Symbol(sym) => {
                matches!(other, ExecutionResult::Symbol(sym2) if sym == sym2)
            }
            ExecutionResult::Func(_, _, faddr) => {
                matches!(other, ExecutionResult::Func(_,_, saddr) if faddr == saddr)
            }
            ExecutionResult::List(fl) => match other {
                ExecutionResult::List(List::Nil) => matches!(fl, List::Nil),
                ExecutionResult::List(List::Cons(_, _, u)) => {
                    matches!(fl, List::Cons(_, _, uuid) if u == uuid)
                }
                _ => false,
            },
            ExecutionResult::Unit => matches!(other, ExecutionResult::Unit),
            ExecutionResult::EmbeddedFunc(name) => {
                matches!(other, ExecutionResult::EmbeddedFunc(name2) if name == name2)
            }
            ExecutionResult::Undefined => false,
        }
    }
}

impl Const {
    fn into_with_env(self, env: &Env) -> ExecutionResult {
        match self {
            Const::Str(s) => ExecutionResult::String(s.to_string(), env.get_const_str_addr(s)),
            Const::Bool(b) => ExecutionResult::Bool(b),
            Const::Num(n) => ExecutionResult::Number(n),
            Const::Unit => ExecutionResult::Unit,
        }
    }
}

impl Display for ExecutionResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExecutionResult::Number(n) => write!(f, "{}", n),
            ExecutionResult::String(s, _) => write!(f, "\"{}\"", s),
            ExecutionResult::Bool(false) => write!(f, "#f"),
            ExecutionResult::Bool(true) => write!(f, "#t"),
            ExecutionResult::Symbol(s) => write!(f, "{}", s),
            ExecutionResult::Func(_, _, _) => write!(f, "#<procedure>"),
            ExecutionResult::Unit => write!(f, ""),
            ExecutionResult::List(l) => write!(f, "{}", l),
            ExecutionResult::EmbeddedFunc(_) => write!(f, "#<procedure>"),
            ExecutionResult::Undefined => write!(f, "undefined"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum List {
    Cons(Box<ExecutionResult>, Box<ExecutionResult>, u128),
    Nil,
}

impl List {
    pub fn new(result: ExecutionResult) -> Self {
        Self::Cons(
            Box::new(result),
            Box::new(ExecutionResult::List(List::Nil)),
            Uuid::new_v4().as_u128(),
        )
    }

    pub fn cons(left: ExecutionResult, right: ExecutionResult) -> Self {
        Self::Cons(Box::new(left), Box::new(right), Uuid::new_v4().as_u128())
    }

    pub fn car(&self) -> ExecutionResult {
        match self {
            List::Cons(car, _, _) => *car.clone(),
            List::Nil => ExecutionResult::List(List::Nil),
        }
    }

    fn car_ref(&self) -> &ExecutionResult {
        match self {
            List::Cons(car, _, _) => car,
            List::Nil => &ExecutionResult::List(List::Nil),
        }
    }

    pub fn cdr(&self) -> ExecutionResult {
        match self {
            List::Cons(_, cdr, _) => *cdr.clone(),
            List::Nil => ExecutionResult::List(List::Nil),
        }
    }

    fn cdr_ref(&self) -> &ExecutionResult {
        match self {
            List::Cons(_, cdr, _) => cdr,
            List::Nil => &ExecutionResult::List(List::Nil),
        }
    }

    pub fn is_list(&self) -> bool {
        match self {
            List::Cons(_, cdr, _) => match *cdr.clone() {
                ExecutionResult::List(l) => l.is_list(),
                _ => false,
            },
            List::Nil => true,
        }
    }

    pub fn len(&self) -> Result<usize, ()> {
        if !self.is_list() {
            Err(())
        } else {
            Ok(Self::len_(self, 0))
        }
    }

    fn len_(ls: &List, depth: usize) -> usize {
        match ls {
            List::Cons(_, cdr, _) => match *cdr.clone() {
                ExecutionResult::List(l) => Self::len_(&l, depth + 1),
                _ => panic!(),
            },
            List::Nil => depth,
        }
    }

    pub fn last(&self) -> Result<ExecutionResult, &'static str> {
        if matches!(self, List::Nil) {
            Err("null?")
        } else {
            let mut cdr = self.clone();
            loop {
                cdr = if let ExecutionResult::List(ls) = cdr.cdr() {
                    ls
                } else {
                    return Err("list?");
                };
                if matches!(cdr.cdr(), ExecutionResult::List(List::Nil)) {
                    break;
                }
            }
            Ok(ExecutionResult::List(cdr))
        }
    }

    pub fn append(first: List, other: ExecutionResult) -> List {
        let (mut firsts, _) = first.into();
        if let ExecutionResult::List(other) = other {
            let (mut others, other_rest) = other.into();
            firsts.append(&mut others);

            (firsts, other_rest).into()
        } else {
            (firsts, Some(other)).into()
        }
    }

    pub fn equal(&self, other: &List) -> bool {
        let other_car = other.car_ref();
        let other_cdr = other.cdr_ref();
        self.car().equal(other_car) && self.cdr().equal(other_cdr)
    }

    pub fn memq(target: List, result: &ExecutionResult) -> Result<List, ()> {
        if target.is_list() {
            Ok(Self::memq_(target, result))
        } else {
            Err(())
        }
    }

    fn memq_(target: List, result: &ExecutionResult) -> List {
        if target.car_ref().eq(result) {
            target
        } else {
            Self::memq_(
                if let ExecutionResult::List(l) = target.cdr() {
                    l
                } else {
                    panic!()
                },
                result,
            )
        }
    }
}

impl From<List> for ExecutionResult {
    fn from(f: List) -> Self {
        Self::List(f)
    }
}

impl From<(Vec<ExecutionResult>, Option<ExecutionResult>)> for List {
    fn from((ls, rest): (Vec<ExecutionResult>, Option<ExecutionResult>)) -> Self {
        let last = if let Some(rest) = rest {
            rest
        } else {
            List::Nil.into()
        };
        let mut list = last;
        for item in ls.iter().rev() {
            list = ExecutionResult::List(List::Cons(
                Box::new(item.clone()),
                Box::new(list),
                Uuid::new_v4().as_u128(),
            ));
        }

        if let ExecutionResult::List(ls) = list {
            ls
        } else {
            panic!()
        }
    }
}

impl From<List> for (Vec<ExecutionResult>, Option<ExecutionResult>) {
    fn from(f: List) -> Self {
        let mut list = Vec::new();
        let mut cdr = f;
        loop {
            list.push(cdr.car());
            if matches!(cdr.cdr(), ExecutionResult::List(List::Nil)) {
                return (list, None);
            } else if !matches!(cdr.cdr(), ExecutionResult::List(_)) {
                return (list, Some(cdr.cdr()));
            } else {
                cdr = if let ExecutionResult::List(ls) = cdr.cdr() {
                    ls
                } else {
                    panic!()
                }
            }
        }
    }
}

impl Display for List {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let List::Nil = self {
            write!(f, "()")
        } else {
            write!(f, "(")?;
            let (list, rest): (Vec<ExecutionResult>, Option<ExecutionResult>) = self.clone().into();
            write!(
                f,
                "{}",
                list.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(" ")
            )?;
            if let Some(rest) = rest {
                write!(f, " . {})", rest)
            } else {
                write!(f, ")")
            }
        }
    }
}

#[test]
fn test_list() {
    let cons = List::cons(
        ExecutionResult::Number(3.into()),
        ExecutionResult::List(List::cons(
            ExecutionResult::Number(3),
            ExecutionResult::List(List::Nil),
        )),
    );
    assert_eq!(true, cons.is_list());

    let cons = List::cons(ExecutionResult::List(cons), ExecutionResult::Bool(true));
    assert_eq!(false, cons.is_list());

    let vecs = vec![
        ExecutionResult::Number(3),
        ExecutionResult::Number(4),
        ExecutionResult::Number(5),
    ];

    let list: List = (vecs.clone(), None).into();
    assert_eq!(true, list.is_list());

    let not_list: List = (vecs, Some(ExecutionResult::Number(23))).into();
    assert_eq!(false, not_list.is_list());
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

fn execute_number_binary_operation(op_kind: NumOpKind, results: &[ExecutionResult]) -> EResult {
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

fn execute_number_binary_comparison(
    op_kind: NumCompOpKind,
    results: &[ExecutionResult],
) -> EResult {
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
enum Types {
    String,
    Num,
    Symbol,
    Bool,
    Proc,
    List,
    Pair,
    Null,
}

fn execute_type_check(expected: Types, actually: &[ExecutionResult]) -> EResult {
    if actually.len() != 1 {
        Err(("number of arguments needs `1`").to_string())
    } else {
        let f = &actually[0];
        Ok(ExecutionResult::Bool(match expected {
            Types::String => matches!(f, ExecutionResult::String(_, _)),
            Types::Num => matches!(f, ExecutionResult::Number(_)),
            Types::Symbol => matches!(f, ExecutionResult::Symbol(_)),
            Types::Bool => matches!(f, ExecutionResult::Bool(_)),
            Types::Proc => matches!(f, ExecutionResult::Func(_, _, _)),
            Types::List => {
                if let ExecutionResult::List(l) = f {
                    l.is_list()
                } else {
                    false
                }
            }
            Types::Pair => {
                if let ExecutionResult::List(l) = f {
                    !matches!(l, List::Nil)
                } else {
                    false
                }
            }
            Types::Null => {
                matches!(f, ExecutionResult::List(List::Nil))
            }
        }))
    }
}

fn execute_not(vals: &[ExecutionResult]) -> EResult {
    if vals.len() != 1 {
        Err(("number of arguments needs `1`").to_string())
    } else {
        let f = &vals[0];
        Ok(ExecutionResult::Bool(matches!(
            f,
            ExecutionResult::Bool(false)
        )))
    }
}

enum ConvType {
    Str,
    Num,
    Sym,
}

fn execute_conversion(from_ty: ConvType, to_ty: ConvType, vals: &[ExecutionResult]) -> EResult {
    if vals.len() != 1 {
        Err(("number of arguments needs `1`").to_string())
    } else {
        let f = &vals[0];
        match (from_ty, to_ty) {
            (ConvType::Str, ConvType::Num) => {
                if let ExecutionResult::String(s, _) = f {
                    Ok(match s.parse::<i64>() {
                        Ok(s) => ExecutionResult::Number(s),
                        Err(_) => ExecutionResult::Bool(false),
                    })
                } else {
                    Err("expected type is string?, but actually type is diffrent".to_string())
                }
            }
            (ConvType::Num, ConvType::Str) => {
                if let ExecutionResult::Number(s) = f {
                    Ok(ExecutionResult::String(
                        s.to_string(),
                        Uuid::new_v4().as_u128(),
                    ))
                } else {
                    Err("expected type is string?, but actually type is diffrent".to_string())
                }
            }
            (ConvType::Sym, ConvType::Str) => {
                if let ExecutionResult::Symbol(s) = f {
                    Ok(ExecutionResult::String(
                        s.to_string(),
                        Uuid::new_v4().as_u128(),
                    ))
                } else {
                    Err("expected type is symbol?, but actually type is diffrent".to_string())
                }
            }
            (ConvType::Str, ConvType::Sym) => {
                if let ExecutionResult::String(s, _) = f {
                    Ok(ExecutionResult::Symbol(s.to_string()))
                } else {
                    Err("expected type is string?, but actually type is diffrent".to_string())
                }
            }
            _ => panic!(),
        }
    }
}

fn execute_string_append(vals: &[ExecutionResult]) -> EResult {
    let mut sp = String::new();
    for item in vals {
        if let ExecutionResult::String(s, _) = item {
            sp.push_str(s);
        } else {
            return Err("string-append needs string?".to_string());
        }
    }
    Ok(ExecutionResult::String(sp, Uuid::new_v4().as_u128()))
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

enum ListOperationKind {
    Car,
    Cdr,
    Cons,
    List,
    Length,
    #[allow(dead_code)]
    Memq,
    Last,
    Append,
    SetCar,
    SetCdr,
}

fn execute_list_operation(operation_kind: ListOperationKind, vals: &[ExecutionResult]) -> EResult {
    match operation_kind {
        // TODO: if values if null?, it returns nil in car or cdr
        ListOperationKind::Car => {
            if vals.len() != 1 {
                Err(("number of arguments needs `1`").to_string())
            } else if let ExecutionResult::List(ls) = &vals[0] {
                Ok(ls.car())
            } else {
                Err("expected type is pair?, but actually type is diffrent".to_string())
            }
        }
        ListOperationKind::Cdr => {
            if vals.len() != 1 {
                Err(("number of arguments needs `1`").to_string())
            } else if let ExecutionResult::List(ls) = &vals[0] {
                Ok(ls.cdr())
            } else {
                Err("expected type is pair?, but actually type is diffrent".to_string())
            }
        }
        ListOperationKind::Cons => {
            if vals.len() != 2 {
                Err(("number of arguments needs `2`").to_string())
            } else {
                Ok(List::Cons(
                    Box::new(vals[0].clone()),
                    Box::new(vals[1].clone()),
                    Uuid::new_v4().as_u128(),
                )
                .into())
            }
        }
        ListOperationKind::List => {
            let list: List = (vals.to_vec(), None).into();
            Ok(list.into())
        }
        ListOperationKind::Length => {
            if vals.len() != 1 {
                Err(("number of arguments needs `1`").to_string())
            } else if let ExecutionResult::List(ls) = &vals[0] {
                match ls.len() {
                    Ok(l) => Ok(ExecutionResult::Number(l as i64)),
                    Err(_) => {
                        Err("expected type is list?, but actually type is diffrent".to_string())
                    }
                }
            } else {
                Err("expected type is list?, but actually type is diffrent".to_string())
            }
        }
        ListOperationKind::Memq => {
            if vals.len() != 2 {
                Err(("number of arguments needs `2`").to_string())
            } else {
                let list = if let ExecutionResult::List(ls) = vals[0].clone() {
                    ls
                } else {
                    return Err("expected type is list?, but actually type is diffrent".to_string());
                };
                match List::memq(list, &vals[1]) {
                    Ok(o) => Ok(ExecutionResult::List(o)),
                    Err(_) => {
                        Err("expected type is list?, but actually type is diffrent".to_string())
                    }
                }
            }
        }
        ListOperationKind::Last => {
            if vals.len() != 1 {
                Err(("number of arguments needs `1`").to_string())
            } else if let ExecutionResult::List(ls) = &vals[0] {
                ls.last().map_err(|_| {
                    "expected type is list?, but actually type is diffrent".to_string()
                })
            } else {
                Err("expected type is list?, but actually type is diffrent".to_string())
            }
        }
        ListOperationKind::Append => {
            if vals.is_empty() {
                Ok(ExecutionResult::List(List::Nil))
            } else if vals.len() == 1 {
                Ok(vals[0].clone())
            } else {
                let first = &vals[0];
                let mid = &vals[1..vals.len() - 1];

                if mid.iter().any(|x| {
                    if let ExecutionResult::List(l) = x {
                        !l.is_list()
                    } else {
                        true
                    }
                }) {
                    return Err(
                        "expected type in arguments is list?, but actually type is diffrent"
                            .to_string(),
                    );
                }
                if let ExecutionResult::List(l) = first {
                    if !l.is_list() {
                        return Err("expected type in first argument is list?, but actually type is diffrent".to_string());
                    }
                    let mut next = l.clone();
                    for item in &vals[1..] {
                        next = List::append(next, item.clone());
                    }
                    Ok(ExecutionResult::List(next))
                } else {
                    Err(
                        "expected type in first argument is list?, but actually type is diffrent"
                            .to_string(),
                    )
                }
            }
        }
        ListOperationKind::SetCar => {
            if vals.len() != 2 {
                Err(("number of arguments needs `2`").to_string())
            } else {
                let (first, uuid) = if let ExecutionResult::List(ls) = &vals[0] {
                    if let List::Cons(_, _, uuid) = ls {
                        (ls, uuid)
                    } else {
                        return Err(
                            "expected type is pair?, but actually type is different".to_string()
                        );
                    }
                } else {
                    return Err(
                        "expected type is pair?, but actually type is different".to_string()
                    );
                };
                Ok(ExecutionResult::List(List::Cons(
                    Box::new(vals[1].clone()),
                    Box::new(first.cdr()),
                    *uuid,
                )))
            }
        }
        ListOperationKind::SetCdr => {
            if vals.len() != 2 {
                Err(("number of arguments needs `2`").to_string())
            } else {
                let (first, uuid) = if let ExecutionResult::List(ls) = &vals[0] {
                    if let List::Cons(_, _, uuid) = ls {
                        (ls, uuid)
                    } else {
                        return Err(
                            "expected type is pair?, but actually type is different".to_string()
                        );
                    }
                } else {
                    return Err(
                        "expected type is pair?, but actually type is different".to_string()
                    );
                };
                Ok(ExecutionResult::List(List::Cons(
                    Box::new(first.car()),
                    Box::new(vals[1].clone()),
                    *uuid,
                )))
            }
        }
    }
}

enum EqCompKind {
    Eq, // shallow compare
    Neq,
    Equal, // deep compare
}

fn execute_comp_operation(op_kind: EqCompKind, vals: &[ExecutionResult]) -> EResult {
    if vals.len() != 2 {
        Err(("number of arguments needs `2`").to_string())
    } else {
        let first = &vals[0];
        let second = &vals[1];
        match op_kind {
            EqCompKind::Eq => Ok(ExecutionResult::Bool(first.eq(second))),
            EqCompKind::Neq => Ok(ExecutionResult::Bool(!first.eq(second))),
            EqCompKind::Equal => Ok(ExecutionResult::Bool(first.equal(second))),
        }
    }
}