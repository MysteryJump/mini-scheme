use std::{
    collections::{HashMap, HashSet},
    sync::Arc,
};

use crate::{
    ast::{Arg, Bindings, Body, Define, Expr, TopLevel},
    interpreter::{Env, ExecutionResult, Interpreter},
};

pub struct Reactive {
    dependency_pairs: HashMap<String, Vec<String>>,
    topo_order: Vec<String>,
    before_values: HashMap<String, ExecutionResult>,
    flow_exprs: HashMap<String, Expr>,
    ins: HashSet<String>,
}

impl Reactive {
    pub fn new(initials: &Bindings, variables: Bindings) -> Result<Self, ()> {
        let (conf, mut used_imports) =
            initials
                .0
                .iter()
                .fold((true, Vec::new()), |(cur_r, mut cur_names), next| {
                    cur_names.push(&next.0 as &str);
                    let cker = VariableUsagesChecker::new(&[]);
                    (
                        cker.check_expr_variables_usages(&next.1).0 && cur_r,
                        cur_names,
                    )
                });
        if !conf || used_imports.iter().collect::<HashSet<_>>().len() != used_imports.len() {
            return Err(());
        }

        used_imports.extend(variables.0.iter().map(|x| &x.0 as &str).collect::<Vec<_>>());

        let mut graph = petgraph::Graph::new();

        let nodes = used_imports
            .iter()
            .map(|x| (*x, graph.add_node(*x)))
            .collect::<HashMap<_, _>>();
        let rev_nodes = nodes
            .iter()
            .map(|x| (*x.1, <&str>::clone(x.0)))
            .collect::<HashMap<_, _>>();
        for (id, expr) in &variables.0 {
            let cker = VariableUsagesChecker::new(&used_imports);
            let (result, useds) = cker.check_expr_variables_usages(expr);
            if !result {
                return Err(());
            }
            for used in useds {
                let ins = nodes[used];
                let outs = nodes[id as &str];
                graph.add_edge(ins, outs, 1);
            }
        }
        let mut topo = petgraph::visit::Topo::new(&graph);
        let mut sorted = Vec::new();
        while let Some(c) = topo.next(&graph) {
            sorted.push(rev_nodes[&c].to_string());
        }
        let mut deps_map = HashMap::new();
        for (id, _) in &initials.0 {
            let mut dfs = petgraph::visit::Dfs::new(&graph, nodes[id as &str]);
            let mut deps = Vec::new();
            while let Some(item) = dfs.next(&graph) {
                let node = rev_nodes[&item];
                if id as &str != node {
                    deps.push(node.to_string());
                }
            }
            deps_map.insert(id.to_string(), deps);
        }

        let flow_exprs = variables.0.into_iter().collect();

        let mut slf = Self {
            dependency_pairs: deps_map,
            before_values: HashMap::new(),
            topo_order: sorted,
            flow_exprs,
            ins: initials.0.iter().map(|x| x.0.clone()).collect(),
        };
        let bf = slf.calculate_initial_values(initials);
        slf.before_values = bf;
        Ok(slf)
    }

    fn calculate_initial_values(&self, initials: &Bindings) -> HashMap<String, ExecutionResult> {
        let mut values = HashMap::new();
        let env = Env::new(Arc::new(|_| {}), None);
        for (id, expr) in &initials.0 {
            let mut inter = Interpreter::new(Arc::new(|_| {}), None);
            let r = inter.execute_toplevel(TopLevel::Expr(expr.clone()));
            let r = if let either::Either::Left(r) = r {
                r.unwrap()
            } else {
                panic!()
            };
            env.add_define(id.clone(), r.clone());
            values.insert(id.clone(), r);
        }

        let topo = self
            .topo_order
            .iter()
            .filter(|x| !values.contains_key(*x))
            .collect::<Vec<_>>();

        let mut env = env;
        for sort in topo {
            let expr = self.flow_exprs[sort].clone();
            let mut inter = Interpreter::with_env(env);
            let result =
                if let either::Either::Left(Ok(l)) = inter.execute_toplevel(TopLevel::Expr(expr)) {
                    l
                } else {
                    panic!();
                };
            env = inter.get_env();
            env.add_define(sort.clone(), result.clone());
            values.insert(sort.clone(), result);
        }
        values
    }

    pub fn update_value(&mut self, id: &str, value: ExecutionResult) -> Result<(), ()> {
        if !self.ins.contains(id) {
            return Err(());
        }
        let deps = &self.dependency_pairs[id];
        let mut env = Env::new(Arc::new(|_| {}), None);
        env.add_define(id.to_string(), value.clone());
        self.before_values.insert(id.to_string(), value);
        for item in &self.topo_order {
            if id == item {
                continue;
            } else if deps.contains(item) {
                let mut ipr = Interpreter::with_env(env);
                let r = ipr.execute_toplevel(TopLevel::Expr(self.flow_exprs[item].clone()));
                env = ipr.get_env();
                if let either::Either::Left(Ok(r)) = r {
                    env.add_define(item.to_string(), r.clone());
                    self.before_values.insert(item.clone(), r.clone()).unwrap();
                } else {
                    panic!();
                }
            } else {
                env.add_define(item.to_string(), self.before_values[item].clone());
            }
        }
        Ok(())
    }

    pub fn get_value(&self, id: &str) -> Option<ExecutionResult> {
        self.before_values.get(id).cloned()
    }
}

#[derive(Debug)]
struct VariableUsagesChecker<'a> {
    used_variables: Vec<&'a str>,
    allowed_variables: &'a [&'a str],
    builtins: &'a [&'static str],
}

impl<'a> VariableUsagesChecker<'a> {
    fn new(allowed_variables: &'a [&str]) -> Self {
        Self {
            used_variables: Vec::new(),
            allowed_variables,
            builtins: &[
                "number?",
                "+",
                "-",
                "*",
                "/",
                "=",
                "<",
                "<=",
                ">",
                ">=",
                "null?",
                "pair?",
                "list?",
                "symbol?",
                "car",
                "cdr",
                "cons",
                "list",
                "length",
                "memq",
                "last",
                "append",
                "set-car!",
                "set-cdr!",
                "boolean?",
                "not",
                "string?",
                "string-append",
                "symbol->string",
                "string->symbol",
                "string->number",
                "number->string",
                "eq?",
                "neq?",
                "equal?",
            ],
        }
    }

    fn check_expr_variables_usages(mut self, expr: &'a Expr) -> (bool, Vec<&'a str>) {
        (
            self.check_expr_variables_usages_(expr, self.allowed_variables),
            self.used_variables,
        )
    }

    fn check_expr_variables_usages_(
        &mut self,
        expr: &'a Expr,
        allowed_variables: &[&'a str],
    ) -> bool {
        match expr {
            Expr::Const(_) => true,
            Expr::Id(id) => {
                if self.allowed_variables.contains(&(id as &str))
                    && !self.builtins.contains(&(id as &str))
                {
                    self.used_variables.push(id)
                }
                allowed_variables.contains(&(id as &str)) || self.builtins.contains(&(id as &str))
            }
            Expr::Lambda(l, x) => {
                let mut allowed = allowed_variables.to_vec();
                match l {
                    Arg::Id(id) => {
                        allowed.push(&id);
                        self.check_body_variables_usages(x, &allowed)
                    }
                    Arg::IdList(ids, opt) => {
                        if let Some(opt) = opt {
                            allowed.push(opt as &str);
                        }
                        ids.iter().for_each(|x| allowed.push(&x));
                        self.check_body_variables_usages(x, &allowed)
                    }
                }
            }
            Expr::Apply(e, ars) => {
                self.check_expr_variables_usages_(e, allowed_variables)
                    && ars
                        .iter()
                        .all(|x| self.check_expr_variables_usages_(x, allowed_variables))
            }
            Expr::Quote(_) => true,
            Expr::Set(v, expr) => {
                self.used_variables.push(&(v as &str));
                allowed_variables.contains(&(v as &str))
                    && self.check_expr_variables_usages_(expr, allowed_variables)
            }
            Expr::Let(name, binds, body) => {
                let mut allowed = allowed_variables.to_vec();
                if let Some(name) = name {
                    allowed.push(name);
                }
                binds.0.iter().all(|x| {
                    allowed.push(&x.0);
                    self.check_expr_variables_usages_(&x.1, allowed_variables)
                }) && self.check_body_variables_usages(body, &allowed)
            }
            Expr::LetStar(binds, body) | Expr::LetRec(binds, body) => {
                let mut allowed = allowed_variables.to_vec();

                binds.0.iter().all(|x| {
                    allowed.push(&x.0);
                    self.check_expr_variables_usages_(&x.1, allowed_variables)
                }) && self.check_body_variables_usages(body, &allowed)
            }
            Expr::If(c, t, e) => {
                self.check_expr_variables_usages_(c, allowed_variables)
                    && self.check_expr_variables_usages_(t, allowed_variables)
                    && if let Some(e) = e {
                        self.check_expr_variables_usages_(e, allowed_variables)
                    } else {
                        true
                    }
            }
            Expr::Cond(c) => c.0.iter().all(|x| {
                self.check_expr_variables_usages_(&x.0, allowed_variables)
                    && x.1
                        .iter()
                        .all(|x| self.check_expr_variables_usages_(x, allowed_variables))
            }),
            Expr::And(es) | Expr::Or(es) | Expr::Begin(es) => es
                .iter()
                .all(|x| self.check_expr_variables_usages_(x, allowed_variables)),
            Expr::Do(d) => {
                d.0.iter().all(|x| {
                    self.check_expr_variables_usages_(&x.1, allowed_variables)
                        && self.check_expr_variables_usages_(&x.2, allowed_variables)
                }) && self.check_expr_variables_usages_(&d.1, allowed_variables)
                    && d.2
                        .iter()
                        .all(|x| self.check_expr_variables_usages_(x, allowed_variables))
                    && self.check_body_variables_usages(&d.3, allowed_variables)
            }
        }
    }

    fn check_body_variables_usages(
        &mut self,
        body: &'a Body,
        allowed_variables: &[&'a str],
    ) -> bool {
        let mut allowed = allowed_variables.to_vec();

        body.0.iter().all(|x| match x {
            Define::Define(id, expr) => {
                let result = self.check_expr_variables_usages_(expr, &allowed);
                allowed.push(id);
                result
            }
            Define::DefineList((id, args, opt_arg), body) => {
                let mut allowed2 = allowed.clone();
                args.iter().for_each(|x| allowed2.push(x));
                if let Some(opt_arg) = opt_arg {
                    allowed2.push(opt_arg);
                }
                let result = self.check_body_variables_usages(body, &allowed2);
                allowed.push(id);
                result
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_reactive() {
        let initials = Bindings(vec![
            ("a".to_string(), Expr::Const(3.into())),
            ("d".to_string(), Expr::Const(5.into())),
            ("e".to_string(), Expr::Const(3.into())),
        ]);
        let flows = Bindings(vec![
            (
                "b".to_string(),
                Expr::Apply(
                    Box::new(Expr::Id("+".to_string())),
                    vec![Expr::Const(3.into()), Expr::Id("a".to_string())],
                ),
            ),
            (
                "c".to_string(),
                Expr::Apply(
                    Box::new(Expr::Id("*".to_string())),
                    vec![Expr::Id("d".to_string()), Expr::Id("a".to_string())],
                ),
            ),
            (
                "g".to_string(),
                Expr::Apply(
                    Box::new(Expr::Id("+".to_string())),
                    vec![Expr::Id("c".to_string()), Expr::Id("b".to_string())],
                ),
            ),
            (
                "h".to_string(),
                Expr::Apply(
                    Box::new(Expr::Id("*".to_string())),
                    vec![Expr::Const(4.into()), Expr::Id("b".to_string())],
                ),
            ),
            (
                "f".to_string(),
                Expr::Apply(
                    Box::new(Expr::Id("number->string".to_string())),
                    vec![Expr::Const(4.into()), Expr::Id("e".to_string())],
                ),
            ),
        ]);

        Reactive::new(&initials, flows).unwrap();
    }
}
