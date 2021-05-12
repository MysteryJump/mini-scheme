use std::sync::{Arc, Mutex};

use once_cell::sync::Lazy;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_namespace = console)]
    fn log(s: &str);
}

#[wasm_bindgen]
pub fn execute_lines(lines: &str) -> String {
    RESULT.lock().unwrap().clear();

    mini_scheme::execute(
        lines,
        Arc::new(|x| {
            RESULT.lock().unwrap().push_str(&x);
        }),
    );

    RESULT.lock().unwrap().clone()
}

static RESULT: Lazy<Mutex<String>> = Lazy::new(|| Mutex::new(String::new()));
