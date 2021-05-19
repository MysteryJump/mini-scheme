use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex,
    },
};
#[cfg(feature = "concurrent")]
use std::{thread, time::Duration};

use either::Either;
use num_rational::Rational64;
use uuid::Uuid;

#[cfg(feature = "concurrent")]
use tokio::sync::mpsc::{Receiver, Sender};

#[cfg(feature = "concurrent")]
use crate::lexer;
use crate::{
    ast::{Arg, Body, Const, Define, Expr, SExpr, TopLevel},
    builtins::{
        execute_conversion, execute_not, execute_number_binary_comparison,
        execute_number_binary_operation, execute_string_append, execute_type_check, ConvType,
        NumCompOpKind, NumOpKind, Types,
    },
    reactive::Reactive,
};

pub type EResult = Result<ExecutionResult, String>;

macro_rules! add_builtin {
    ($map:ident,$($name:expr),*) => {
        $(
            $map.insert($name);
        )*
    };
}

#[cfg(feature = "concurrent")]
type ActorResult = (u128, Vec<ExecutionResult>);

#[cfg(feature = "concurrent")]
#[derive(Debug)]
pub struct Actor {
    results: Arc<Mutex<HashMap<u128, EResult>>>,
    name: String,
    lambda: Expr,
    receriver: Arc<Mutex<Receiver<ActorResult>>>,
    interpreter: Arc<Mutex<Interpreter>>,
    handle: Mutex<Option<std::thread::JoinHandle<()>>>,
}

#[cfg(feature = "concurrent")]
impl Actor {
    pub fn run(&self) {
        let receiver = self.receriver.clone();
        let interpreter = self.interpreter.clone();
        let lambda = self.lambda.clone();
        let results = self.results.clone();
        let handle = thread::spawn(move || {
            let recver = receiver;
            {
                let mut recver = recver.lock().unwrap();
                while let Some((mes_id, mes_body)) = recver.blocking_recv() {
                    let result = interpreter.lock().unwrap().execute_expr(Expr::Apply(
                        Box::new(lambda.clone()),
                        mes_body.into_iter().map(|x| x.into()).collect(),
                    ));
                    results.lock().unwrap().insert(mes_id, result);
                }
            }
        });
        self.handle.lock().unwrap().replace(handle);
    }

    pub fn abort(&self) {
        todo!()
    }

    pub fn new(
        name: String,
        args: Arg,
        body: Body,
        current_env: Env,
        actor_map: Arc<Mutex<ActorMap>>,
    ) -> (Self, Sender<(u128, Vec<ExecutionResult>)>) {
        let lambda = Expr::Lambda(args, body);
        let (sender, recvr) = tokio::sync::mpsc::channel(100);
        (
            Self {
                results: Arc::new(Mutex::new(HashMap::new())),
                name,
                lambda,
                interpreter: Arc::new(Mutex::new(Interpreter::with_env_and_actor_map(
                    current_env,
                    actor_map,
                ))),
                receriver: Arc::new(Mutex::new(recvr)),
                handle: Mutex::new(None),
            },
            sender,
        )
    }
}

#[cfg(feature = "concurrent")]
#[derive(Debug, Default)]
pub struct ActorMap {
    pub map: HashMap<String, (Actor, Sender<ActorResult>)>,
    pub message_id_name_pairs: Mutex<HashMap<u128, String>>,
}

#[cfg(feature = "concurrent")]
impl ActorMap {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn define_and_run(&mut self, actor: Actor, sender: Sender<ActorResult>) {
        actor.run();
        self.map.insert(actor.name.clone(), (actor, sender));
    }

    pub fn abort(&self, name: String) {
        self.map[&name].0.abort();
    }

    pub fn send_message(&self, name: String, message: Vec<ExecutionResult>) -> u128 {
        let uuid = Uuid::new_v4().as_u128();
        self.map[&name].1.blocking_send((uuid, message)).unwrap();
        self.message_id_name_pairs
            .lock()
            .unwrap()
            .insert(uuid, name);
        uuid
    }

    pub fn get_result(&self, id: u128) -> Option<EResult> {
        let name = &self.message_id_name_pairs.lock().unwrap()[&id];
        let r = self.map[name].0.results.lock().unwrap().get(&id).cloned();
        r
    }
}

pub struct Env {
    toplevel: Arc<Flame>,
    current_flame: Arc<Flame>,
    used_builtins: Mutex<HashSet<&'static str>>,
    builtins: HashSet<&'static str>,
    stdout: Arc<dyn Fn(String) + Sync + Send>,
    cancellation_token: Option<Arc<AtomicBool>>,
    reactive_properties: Option<Arc<Mutex<Reactive>>>,
    is_newborn: bool,
    str_consts_pairs: Mutex<HashMap<String, u128>>,
}

#[derive(Debug)]
pub struct Flame {
    parent: Option<Arc<Flame>>,
    defineds: Mutex<HashMap<String, ExecutionResult>>,
}

impl Clone for Flame {
    fn clone(&self) -> Self {
        Self {
            parent: self.parent.clone(),
            defineds: Mutex::new(self.defineds.lock().unwrap().clone()),
        }
    }
}

impl Env {
    pub fn new(
        stdout: Arc<dyn Fn(String) + Sync + Send>,
        cancellation_token: Option<Arc<AtomicBool>>,
    ) -> Self {
        let mut builtins = HashSet::new();
        add_builtin!(
            builtins, "number?", "integer?", "+", "-", "*", "/", "=", "<", "<=", ">", ">="
        );
        add_builtin!(
            builtins, "null?", "pair?", "list?", "symbol?", "car", "cdr", "cons", "list", "length",
            "memq", "last", "append", "set-car!", "set-cdr!"
        );
        add_builtin!(builtins, "boolean?", "not");
        add_builtin!(
            builtins,
            "string?",
            "string-append",
            "symbol->string",
            "string->symbol",
            "string->number",
            "number->string"
        );
        add_builtin!(builtins, "procedure?");
        add_builtin!(builtins, "eq?", "neq?", "equal?");
        add_builtin!(builtins, "load");
        add_builtin!(builtins, "display");

        #[cfg(feature = "concurrent")]
        add_builtin!(builtins, "send-message", "get-result", "await", "sleep");

        let toplevel = Arc::new(Flame::new(None));
        Self {
            current_flame: toplevel.clone(),
            toplevel,
            used_builtins: Mutex::new(HashSet::new()),
            builtins,
            stdout,
            cancellation_token,
            reactive_properties: None,
            is_newborn: true,
            str_consts_pairs: Mutex::new(HashMap::new()),
        }
    }
    fn create_and_enter_flame(&mut self) {
        let flame = Flame::create_child(self.current_flame.clone());
        self.current_flame = Arc::new(flame);
    }
    fn break_flame(&mut self) {
        let parent = self.current_flame.parent.as_ref().unwrap();
        self.current_flame = parent.clone();
    }
    fn get_flame(&self) -> Arc<Flame> {
        self.current_flame.clone()
    }
    fn replace_flame(&mut self, flame: Arc<Flame>) -> Arc<Flame> {
        let current = self.current_flame.clone();
        self.current_flame = flame;
        current
    }
    fn update_define(&self, name: &str, value: ExecutionResult) -> Result<(), ()> {
        if let Some(react) = &self.reactive_properties {
            if react
                .lock()
                .unwrap()
                .update_value(name, value.clone())
                .is_ok()
            {
                return Ok(());
            };
        }

        let mut cdefs = self.current_flame.defineds.lock().unwrap();

        if cdefs.contains_key(name) {
            cdefs.insert(name.to_string(), value);
            Ok(())
        } else {
            let mut parent = self.current_flame.clone();
            while let Some(s) = parent.parent.clone() {
                let mut cdefs = s.defineds.lock().unwrap();

                if cdefs.contains_key(name) {
                    cdefs.insert(name.to_string(), value);
                    return Ok(());
                }
                parent = s.clone();
            }
            Err(())
        }
    }
    pub fn add_define(&self, name: &str, value: ExecutionResult) {
        let mut cdefs = self.current_flame.defineds.lock().unwrap();
        if let Some(s) = self.builtins.get(name) {
            self.used_builtins.lock().unwrap().insert(s);
        }
        cdefs.insert(name.to_string(), value);
    }
    pub fn add_defines(&self, pairs: Vec<(&str, ExecutionResult)>) {
        pairs
            .into_iter()
            .for_each(|(name, value)| self.add_define(name, value));
    }
    pub fn get_define(&self, name: &str) -> Option<ExecutionResult> {
        if let Some(s) = self.builtins.get(name) {
            if !self.used_builtins.lock().unwrap().contains(name) {
                return Some(ExecutionResult::EmbeddedFunc(s));
            }
        }
        if let Some(react) = &self.reactive_properties {
            if let Some(result) = react.lock().unwrap().get_value(name) {
                return Some(result);
            }
        }
        let cdefs = self.current_flame.defineds.lock().unwrap();
        if !cdefs.contains_key(name) {
            let mut c = self.current_flame.clone();
            while let Some(parent) = &c.parent.clone() {
                let cdefs = parent.defineds.lock().unwrap();
                if cdefs.contains_key(name) {
                    return Some(cdefs[name].clone());
                }
                c = parent.clone();
            }
            None
        } else {
            Some(cdefs[name].clone())
        }
    }
    pub fn get_const_str_addr(&self, name: String) -> u128 {
        #[allow(clippy::map_entry)]
        if self.str_consts_pairs.lock().unwrap().contains_key(&name) {
            self.str_consts_pairs.lock().unwrap()[&name]
        } else {
            let addr = Uuid::new_v4().as_u128();
            self.str_consts_pairs.lock().unwrap().insert(name, addr);
            addr
        }
    }
    pub fn set_reactive(&mut self, reactive: Arc<Mutex<Reactive>>) {
        if self.reactive_properties.is_none() {
            self.reactive_properties = Some(reactive);
        }
    }
    pub fn replace_to_toplevel(&mut self) {
        self.current_flame = self.toplevel.clone();
    }
}

impl Flame {
    fn new(parent: Option<Arc<Flame>>) -> Self {
        Self {
            parent,
            defineds: Mutex::new(HashMap::new()),
        }
    }

    fn create_child(self: Arc<Self>) -> Self {
        Self::new(Some(self))
    }
}

impl std::fmt::Debug for Env {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::result::Result<(), std::fmt::Error> {
        f.debug_struct("Env")
            .field("current_flame", &self.current_flame)
            .field("str_consts_pairs", &self.str_consts_pairs)
            .finish()
    }
}

impl Clone for Env {
    fn clone(&self) -> Self {
        Self {
            str_consts_pairs: Mutex::new(self.str_consts_pairs.lock().unwrap().clone()),
            stdout: self.stdout.clone(),
            cancellation_token: self.cancellation_token.clone(),
            reactive_properties: self.reactive_properties.clone(),
            is_newborn: self.is_newborn,
            current_flame: self.current_flame.clone(),
            used_builtins: Mutex::new(self.used_builtins.lock().unwrap().clone()),
            builtins: self.builtins.clone(),
            toplevel: self.toplevel.clone(),
        }
    }
}

#[derive(Debug)]
pub struct Interpreter {
    env: Env,
    #[cfg(feature = "concurrent")]
    actor_map: Arc<Mutex<ActorMap>>,
}

impl Interpreter {
    pub fn new(
        logger: Arc<dyn Fn(String) + Sync + Send>,
        cancellation_token: Option<Arc<AtomicBool>>,
    ) -> Self {
        Self {
            env: Env::new(logger, cancellation_token),
            #[cfg(feature = "concurrent")]
            actor_map: Arc::new(Mutex::new(ActorMap::new())),
        }
    }

    pub fn with_env(env: Env) -> Self {
        Self {
            env,
            #[cfg(feature = "concurrent")]
            actor_map: Arc::new(Mutex::new(ActorMap::new())),
        }
    }

    #[cfg(feature = "concurrent")]
    pub fn with_env_and_actor_map(env: Env, actor_map: Arc<Mutex<ActorMap>>) -> Self {
        Self { env, actor_map }
    }

    pub fn get_env(self) -> Env {
        self.env
    }

    pub fn execute_toplevel(&mut self, toplevel: TopLevel) -> Either<EResult, Vec<EResult>> {
        self.env.replace_to_toplevel();
        let r = match toplevel {
            TopLevel::Expr(e) => Either::Left(self.execute_expr(e)),
            TopLevel::Define(def) => Either::Left(self.execute_define(def)),
            TopLevel::DefineActor((name, args, rest_arg), body) => {
                #[cfg(not(feature = "concurrent"))]
                return Either::Left(Err(
                    "Unsupported feature usage in this platform.".to_string()
                ));

                #[cfg(feature = "concurrent")]
                {
                    let (actor, sender) = Actor::new(
                        name,
                        Arg::IdList(args, rest_arg),
                        body,
                        self.env.clone(),
                        self.actor_map.clone(),
                    );
                    self.actor_map.lock().unwrap().define_and_run(actor, sender);
                    Either::Left(Ok(ExecutionResult::Unit))
                }
            }
            TopLevel::DefineProperties(ins, flows) => {
                if self.env.is_newborn {
                    let reactive = match Reactive::new(&ins, flows)
                        .map_err(|_| "Cannot analyze reactive properties.".to_string())
                    {
                        Ok(r) => r,
                        Err(e) => return Either::Left(Err(e)),
                    };
                    self.env.set_reactive(Arc::new(Mutex::new(reactive)));
                    Either::Left(Ok(ExecutionResult::Unit))
                } else {
                    panic!();
                }
            }
            TopLevel::Load(path) => {
                #[cfg(not(feature = "concurrent"))]
                return Either::Left(Err(
                    "Unsupported function usage in this platform.".to_string()
                ));

                #[cfg(feature = "concurrent")]
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
                    while let Some(pp) = parser.parse_toplevel().unwrap() {
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
        };
        self.env.is_newborn = false;
        r
    }

    fn execute_expr(&mut self, mut expr: Expr) -> EResult {
        let mut tail_depth = 0;
        let mut saved_flame = None;
        loop {
            if let Some(tk) = self.env.cancellation_token.as_ref() {
                if tk.load(Ordering::Relaxed) {
                    tk.store(false, Ordering::SeqCst);
                    return Err("Interrupted by user".to_string());
                }
            }
            let r = match &expr {
                Expr::Const(c) => Ok(c.clone().into_with_env(&self.env)),
                Expr::Id(id) => self
                    .env
                    .get_define(id)
                    .ok_or_else(|| "Cannot find such name.".to_string()),
                Expr::Lambda(arg, body) => Ok(ExecutionResult::Func(
                    arg.clone(),
                    body.clone(),
                    Uuid::new_v4().as_u128(),
                    self.env.get_flame(),
                )),
                Expr::Apply(func, arg_apply) => {
                    let result_func = self.execute_expr(*func.clone())?;
                    match result_func {
                        ExecutionResult::Func(arg_func, body, _, flame) => {
                            let r = match arg_func {
                                Arg::Id(id) => {
                                    if arg_apply.len() != 1 {
                                        Err("Args count is not match".to_string())
                                    } else {
                                        let r = self.execute_expr(arg_apply[0].clone())?;
                                        if saved_flame.is_none() {
                                            saved_flame = Some(self.env.replace_flame(flame));
                                        } else {
                                            self.env.replace_flame(flame);
                                        }
                                        self.env.create_and_enter_flame();
                                        self.env.add_define(&id, r);
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
                                            let executed_results = ids
                                                .iter()
                                                .zip(arg_apply)
                                                .map(|(name, expr)| {
                                                    (name, self.execute_expr(expr.clone()))
                                                })
                                                .collect::<Vec<_>>();

                                            let args = executed_results
                                                .iter()
                                                .take(ids.len())
                                                .collect::<Vec<_>>();

                                            let mut rests = Vec::new();
                                            for (_, r) in executed_results.iter().skip(ids_len) {
                                                rests.push(r.clone()?);
                                            }
                                            let list = ExecutionResult::List((rests, None).into());

                                            if saved_flame.is_none() {
                                                saved_flame = Some(self.env.replace_flame(flame));
                                            } else {
                                                self.env.replace_flame(flame);
                                            }
                                            self.env.create_and_enter_flame();
                                            for (name, result) in args {
                                                self.env.add_define(name, result.clone()?);
                                            }

                                            self.env.add_define(&rest, list);

                                            let result = self.execute_body_with_tail(body)?;
                                            expr = result;
                                            tail_depth += 1;
                                            continue;
                                        }
                                    } else if ids.len() != arg_apply.len() {
                                        Err("Args count is not match".to_string())
                                    } else {
                                        let mut binds = Vec::new();
                                        for (key, result) in
                                            ids.iter().zip(arg_apply).map(|(name, expr)| {
                                                (name, self.execute_expr(expr.clone()))
                                            })
                                        {
                                            binds.push((key as &str, result?));
                                        }
                                        if saved_flame.is_none() {
                                            saved_flame = Some(self.env.replace_flame(flame));
                                        } else {
                                            self.env.replace_flame(flame);
                                        }
                                        self.env.create_and_enter_flame();
                                        self.env.add_defines(binds);
                                        let result = self.execute_body_with_tail(body)?;
                                        expr = result;
                                        tail_depth += 1;
                                        continue;
                                    }
                                }
                            };
                            if let Some(flame) = saved_flame.clone() {
                                self.env.replace_flame(flame);
                            }
                            r
                        }
                        #[cfg(feature = "concurrent")]
                        ExecutionResult::EmbeddedFunc("send-message") => {
                            if arg_apply.is_empty() {
                                Err("the number of arguments needs 1 at least".to_string())
                            } else if let Expr::Id(id) = arg_apply.first().unwrap() {
                                let evaleds = arg_apply
                                    .iter()
                                    .skip(1)
                                    .map(|x| self.execute_expr(x.clone()))
                                    .collect::<Result<Vec<_>, _>>()?;

                                let id = self
                                    .actor_map
                                    .lock()
                                    .unwrap()
                                    .send_message(id.to_string(), evaleds);
                                Ok(ExecutionResult::ActorResultId(id))
                            } else {
                                Err("the first argument needs id".to_string())
                            }
                        }
                        #[cfg(feature = "concurrent")]
                        ExecutionResult::EmbeddedFunc("stop-actor") => {
                            if arg_apply.is_empty() {
                                Err("the number of arguments needs 1 at least".to_string())
                            } else if let Expr::Id(id) = arg_apply.first().unwrap() {
                                self.actor_map.lock().unwrap().abort(id.to_string());
                                Ok(ExecutionResult::Unit)
                            } else {
                                Err("the first argument needs id".to_string())
                            }
                        }
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
                                "integer?" => execute_type_check(Types::Integer, &evaleds),
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
                                            .update_define(&id, result)
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
                                            .update_define(&id, result)
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
                                        (*self.env.stdout.clone())(evaleds[0].to_string_display());
                                        Ok(ExecutionResult::Unit)
                                    }
                                }
                                #[cfg(feature = "concurrent")]
                                "sleep" => {
                                    if evaleds.is_empty() {
                                        std::thread::sleep(std::time::Duration::from_millis(1000));
                                        Ok(ExecutionResult::Unit)
                                    } else if let ExecutionResult::Number(n) = &evaleds[0] {
                                        std::thread::sleep(std::time::Duration::from_millis(
                                            n.to_integer() as u64,
                                        ));
                                        Ok(ExecutionResult::Unit)
                                    } else {
                                        Err("the number of argument needs 1 or 0 and first-argument needs number?".to_string())
                                    }
                                }
                                #[cfg(feature = "concurrent")]
                                "get-result" => {
                                    if evaleds.is_empty() {
                                        Err("the number of argument needs 1 and first-argument needs actor_id?".to_string())
                                    } else if let ExecutionResult::ActorResultId(id) = &evaleds[0] {
                                        let result = self.actor_map.lock().unwrap().get_result(*id);
                                        match result {
                                            Some(r) => r,
                                            None => Ok(ExecutionResult::Unit),
                                        }
                                    } else {
                                        Err("the number of argument needs 1 and first-argument needs actor_id?".to_string())
                                    }
                                }
                                // TODO: multiple await
                                #[cfg(feature = "concurrent")]
                                "await" => {
                                    if evaleds.is_empty() {
                                        Err("the number of argument needs 1 and first-argument needs actor_id?".to_string())
                                    } else if let ExecutionResult::ActorResultId(id) = &evaleds[0] {
                                        let result;
                                        loop {
                                            let current =
                                                self.actor_map.lock().unwrap().get_result(*id);
                                            match current {
                                                Some(Ok(r)) => {
                                                    result = r;
                                                    break;
                                                }
                                                Some(Err(e)) => return Err(e),
                                                _ => {
                                                    thread::sleep(Duration::from_millis(10));
                                                }
                                            }
                                        }
                                        Ok(result)
                                    } else {
                                        Err("the number of argument needs 1 and first-argument needs actor_id?".to_string())
                                    }
                                }
                                #[cfg(feature = "concurrent")]
                                "actor-id" => {
                                    if evaleds.len() != 1
                                        || matches!(&evaleds[0], ExecutionResult::String(_, _))
                                    {
                                        Err("the number of argument needs 1 or 0 and first-argument needs string?".to_string())
                                    } else {
                                        let s = if let ExecutionResult::String(s, _) = &evaleds[0] {
                                            s
                                        } else {
                                            panic!()
                                        };
                                        match s.parse() {
                                            Ok(o) => Ok(ExecutionResult::ActorResultId(o)),
                                            Err(_) => {
                                                Err("cannot parse string to u128 number"
                                                    .to_string())
                                            }
                                        }
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
                Expr::Set(name, expr) => {
                    let r = self.execute_expr(*expr.clone())?;
                    self.env
                        .update_define(name, r)
                        .map_err(|_| "set expr to undefined name".to_string())
                        .map(|_| ExecutionResult::Unit)
                }
                Expr::Let(name, binds, body) => {
                    let mut calced_bindeds = Vec::new();
                    for (name, expr) in &binds.0 {
                        calced_bindeds.push((name as &str, self.execute_expr(expr.clone())?));
                    }
                    self.env.create_and_enter_flame();
                    self.env.add_defines(calced_bindeds);
                    let result = if let Some(name) = name {
                        let arg_ls = binds.0.iter().map(|x| x.0.clone()).collect();
                        let func = ExecutionResult::Func(
                            Arg::IdList(arg_ls, None),
                            body.clone(),
                            Uuid::new_v4().as_u128(),
                            self.env.get_flame(),
                        );
                        self.env.add_define(name, func);

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
                        let r = self.execute_expr(expr.clone())?;
                        self.env.add_define(name, r);
                        self.env.create_and_enter_flame();
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
                        self.env.create_and_enter_flame();
                        let undefineds = names
                            .iter()
                            .map(|x| (x as &str, ExecutionResult::Undefined))
                            .collect();
                        self.env.add_defines(undefineds);

                        let mut calced_bindeds = Vec::new();
                        for (name, expr) in &binds.0 {
                            calced_bindeds.push((name.clone(), self.execute_expr(expr.clone())?));
                        }
                        for (name, result) in calced_bindeds {
                            self.env.update_define(&name, result).unwrap();
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
                    let mut res = None;
                    for (con_expr, exprs) in cond.0.clone() {
                        let result = self.execute_expr(con_expr)?;
                        if !matches!(result, ExecutionResult::Bool(false)) {
                            let result = exprs
                                .iter()
                                .map(|x| self.execute_expr(x.clone()))
                                .collect::<Result<Vec<_>, _>>()?;
                            res = Some(result.last().unwrap().clone());
                            break;
                        }
                    }
                    if let Some(res) = res {
                        Ok(res)
                    } else if let Some(els) = cond.1.clone() {
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
                    self.env.create_and_enter_flame();
                    let mut map = HashMap::new();
                    let mut binds = Vec::new();
                    for (name, init, step) in &d.0 {
                        binds.push((name as &str, self.execute_expr(init.clone())?));
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
                                    .update_define(&name, result)
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
            if let Some(saved_flame) = saved_flame {
                self.env.replace_flame(saved_flame);
            } else {
                for _ in 0..tail_depth {
                    self.env.break_flame();
                }
            }

            return r;
        }
    }

    fn execute_define(&mut self, define: Define) -> EResult {
        match define {
            Define::Define(name, expr) => {
                let r = self.execute_expr(expr)?;
                self.env.add_define(&name, r);
            }
            Define::DefineList((name, arg, arg_rest), body) => self.env.add_define(
                &name,
                ExecutionResult::Func(
                    Arg::IdList(arg, arg_rest),
                    body,
                    Uuid::new_v4().as_u128(),
                    self.env.get_flame(),
                ),
            ),
        }
        Ok(ExecutionResult::Unit)
    }

    fn execute_body(&mut self, body: Body) -> EResult {
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

    fn execute_body_with_tail(&mut self, body: Body) -> Result<Expr, String> {
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
    Number(Rational64),
    ActorResultId(u128),
    String(String, u128),
    Bool(bool),
    Symbol(String),
    Func(Arg, Body, u128, Arc<Flame>),
    List(List),
    Unit,
    EmbeddedFunc(&'static str),
    Undefined,
    ActorUndefined,
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
            ExecutionResult::Func(_, _, a, _) => {
                matches!(other, ExecutionResult::Func(_,_,aa,_) if a == aa)
            }
            ExecutionResult::List(l) => matches!(other, ExecutionResult::List(ll) if l.equal(ll)),
            ExecutionResult::Unit => matches!(other, ExecutionResult::Unit),
            ExecutionResult::EmbeddedFunc(e) => {
                matches!(other, ExecutionResult::EmbeddedFunc(ee) if e == ee)
            }
            ExecutionResult::ActorResultId(r) => {
                matches!(other, ExecutionResult::ActorResultId(rr) if r == rr)
            }
            ExecutionResult::Undefined => false,
            ExecutionResult::ActorUndefined => false,
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
            ExecutionResult::Func(_, _, faddr, _) => {
                matches!(other, ExecutionResult::Func(_,_, saddr,_) if faddr == saddr)
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
            ExecutionResult::ActorResultId(r) => {
                matches!(other, ExecutionResult::ActorResultId(rr) if r == rr)
            }
            ExecutionResult::Undefined => false,
            ExecutionResult::ActorUndefined => false,
        }
    }

    fn to_string_display(&self) -> String {
        if let ExecutionResult::String(s, _) = self {
            s.to_string()
        } else {
            self.to_string()
        }
    }
}

impl From<ExecutionResult> for Expr {
    fn from(er: ExecutionResult) -> Self {
        match er {
            ExecutionResult::Number(n) => {
                if n.is_integer() {
                    Expr::Const((*n.numer()).into())
                } else {
                    Expr::Apply(
                        Box::new(Expr::Id("/".to_string())),
                        vec![
                            Expr::Const((*n.numer()).into()),
                            Expr::Const((*n.denom()).into()),
                        ],
                    )
                }
            }
            ExecutionResult::String(s, _) => Expr::Const(s.into()),
            ExecutionResult::Bool(b) => Expr::Const(b.into()),
            ExecutionResult::Symbol(s) => Expr::Quote(SExpr::Const(s.into())),
            ExecutionResult::Func(arg, body, _, _) => Expr::Lambda(arg, body),
            ExecutionResult::List(_) => todo!(),
            ExecutionResult::Unit => Expr::Const(().into()),
            ExecutionResult::EmbeddedFunc(s) => Expr::Id(s.to_string()),
            ExecutionResult::Undefined => panic!(),
            ExecutionResult::ActorResultId(_) => {
                panic!();
            }
            ExecutionResult::ActorUndefined => {
                panic!()
            }
        }
    }
}

impl Const {
    fn into_with_env(self, env: &Env) -> ExecutionResult {
        match self {
            Const::Str(s) => ExecutionResult::String(s.to_string(), env.get_const_str_addr(s)),
            Const::Bool(b) => ExecutionResult::Bool(b),
            Const::Num(n) => ExecutionResult::Number(Rational64::from_integer(n)),
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
            ExecutionResult::Func(_, _, _, _) => write!(f, "#<procedure>"),
            ExecutionResult::Unit => write!(f, ""),
            ExecutionResult::List(l) => write!(f, "{}", l),
            ExecutionResult::EmbeddedFunc(_) => write!(f, "#<procedure>"),
            ExecutionResult::Undefined => write!(f, "undefined"),
            ExecutionResult::ActorResultId(r) => write!(f, "#<actor_result:{}>", r),
            ExecutionResult::ActorUndefined => write!(f, "#<actor_void>"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum List {
    Cons(Arc<ExecutionResult>, Arc<ExecutionResult>, u128),
    Nil,
}

impl List {
    pub fn new(result: ExecutionResult) -> Self {
        Self::Cons(
            Arc::new(result),
            Arc::new(ExecutionResult::List(List::Nil)),
            Uuid::new_v4().as_u128(),
        )
    }

    pub fn cons(left: ExecutionResult, right: ExecutionResult) -> Self {
        Self::Cons(Arc::new(left), Arc::new(right), Uuid::new_v4().as_u128())
    }

    pub fn car(&self) -> ExecutionResult {
        match self {
            List::Cons(car, _, _) => (*car.clone()).clone(),
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
            List::Cons(_, cdr, _) => (*cdr.clone()).clone(),
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
            List::Cons(_, cdr, _) => match (*cdr.clone()).clone() {
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
            List::Cons(_, cdr, _) => match (*cdr.clone()).clone() {
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
        if matches!(other_car, ExecutionResult::List(List::Nil))
            && matches!(other_cdr, ExecutionResult::List(List::Nil))
            && matches!(self.car(), ExecutionResult::List(List::Nil))
            && matches!(self.cdr(), ExecutionResult::List(List::Nil))
        {
            true
        } else {
            self.car().equal(other_car) && self.cdr().equal(other_cdr)
        }
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

    fn car_arc_ref(&self) -> (Arc<ExecutionResult>, u128) {
        match self {
            List::Cons(car, _, uuid) => (car.clone(), *uuid),
            List::Nil => panic!(),
        }
    }

    fn cdr_arc_ref(&self) -> (Arc<ExecutionResult>, u128) {
        match self {
            List::Cons(_, cdr, uuid) => (cdr.clone(), *uuid),
            List::Nil => panic!(),
        }
    }

    pub fn set_car(target: &List, result: ExecutionResult) -> List {
        let (cdr, uuid) = target.cdr_arc_ref();
        List::Cons(Arc::new(result), cdr, uuid)
    }

    pub fn set_cdr(target: &List, result: ExecutionResult) -> List {
        let (car, uuid) = target.car_arc_ref();
        List::Cons(car, Arc::new(result), uuid)
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
                Arc::new(item.clone()),
                Arc::new(list),
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
            ExecutionResult::Number(3.into()),
            ExecutionResult::List(List::Nil),
        )),
    );
    assert!(cons.is_list());

    let cons = List::cons(ExecutionResult::List(cons), ExecutionResult::Bool(true));
    assert!(!cons.is_list());

    let vecs = vec![
        ExecutionResult::Number(3.into()),
        ExecutionResult::Number(4.into()),
        ExecutionResult::Number(5.into()),
    ];

    let list: List = (vecs.clone(), None).into();
    assert!(list.is_list());

    let not_list: List = (vecs, Some(ExecutionResult::Number(23.into()))).into();
    assert!(!not_list.is_list());
}

enum ListOperationKind {
    Car,
    Cdr,
    Cons,
    List,
    Length,
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
                    Arc::new(vals[0].clone()),
                    Arc::new(vals[1].clone()),
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
                    Ok(l) => Ok(ExecutionResult::Number((l as i64).into())),
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
                let list = if let ExecutionResult::List(ls) = vals[1].clone() {
                    ls
                } else {
                    return Err("expected type is list?, but actually type is diffrent".to_string());
                };
                match List::memq(list, &vals[0]) {
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
                let (first, _) = if let ExecutionResult::List(ls) = &vals[0] {
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
                Ok(ExecutionResult::List(List::set_car(first, vals[1].clone())))
            }
        }
        ListOperationKind::SetCdr => {
            if vals.len() != 2 {
                Err(("number of arguments needs `2`").to_string())
            } else {
                let (first, _) = if let ExecutionResult::List(ls) = &vals[0] {
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
                Ok(ExecutionResult::List(List::set_cdr(first, vals[1].clone())))
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
