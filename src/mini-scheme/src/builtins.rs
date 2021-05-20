use std::fmt::Display;

use uuid::Uuid;

use crate::interpreter::{EResult, ExecutionResult, List};

#[derive(Debug, Clone, Copy)]
pub enum NumOpKind {
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

pub fn execute_number_binary_operation(op_kind: NumOpKind, results: &[ExecutionResult]) -> EResult {
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
                        if item == &0.into() {
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
pub enum NumCompOpKind {
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

pub fn execute_number_binary_comparison(
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
pub enum Types {
    String,
    Num,
    Symbol,
    Bool,
    Proc,
    List,
    Pair,
    Null,
    Integer,
    Promise,
}

pub fn execute_type_check(expected: Types, actually: &[ExecutionResult]) -> EResult {
    if actually.len() != 1 {
        Err(("number of arguments needs `1`").to_string())
    } else {
        let f = &actually[0];
        Ok(ExecutionResult::Bool(match expected {
            Types::String => matches!(f, ExecutionResult::String(_, _)),
            Types::Num => matches!(f, ExecutionResult::Number(_)),
            Types::Symbol => matches!(f, ExecutionResult::Symbol(_)),
            Types::Bool => matches!(f, ExecutionResult::Bool(_)),
            Types::Proc => matches!(
                f,
                ExecutionResult::Func(_, _, _, _) | ExecutionResult::EmbeddedFunc(_)
            ),
            Types::List => matches!(f, ExecutionResult::List(l) if l.is_list()),
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
            Types::Integer => matches!(f, ExecutionResult::Number(n) if n.is_integer()),
            Types::Promise => matches!(f, ExecutionResult::Promise(_)),
        }))
    }
}

pub fn execute_not(vals: &[ExecutionResult]) -> EResult {
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

pub enum ConvType {
    Str,
    Num,
    Sym,
}

pub fn execute_conversion(from_ty: ConvType, to_ty: ConvType, vals: &[ExecutionResult]) -> EResult {
    if vals.len() != 1 {
        Err(("number of arguments needs `1`").to_string())
    } else {
        let f = &vals[0];
        match (from_ty, to_ty) {
            (ConvType::Str, ConvType::Num) => {
                if let ExecutionResult::String(s, _) = f {
                    Ok(match s.parse::<i64>() {
                        Ok(s) => ExecutionResult::Number(s.into()),
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

pub fn execute_string_append(vals: &[ExecutionResult]) -> EResult {
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
        &vec![
            ExecutionResult::Number(5.into()),
            ExecutionResult::Number(3.into()),
        ],
    )
    .unwrap();
    if let ExecutionResult::Bool(b) = result {
        assert!(!b);
    } else {
        panic!()
    }
}
