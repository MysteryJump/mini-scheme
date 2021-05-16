mod utils;

use std::sync::{Arc, Mutex};

use once_cell::sync::Lazy;
use wasm_bindgen::prelude::*;

// When the `wee_alloc` feature is enabled, use `wee_alloc` as the global
// allocator.
#[cfg(feature = "wee_alloc")]
#[global_allocator]
static ALLOC: wee_alloc::WeeAlloc = wee_alloc::WeeAlloc::INIT;

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);
}

#[wasm_bindgen]
pub fn execute_lines(lines: &str) -> String {
    RESULT.lock().unwrap().clear();

    mini_scheme::execute(
        lines,
        Arc::new(|x| {
            RESULT.lock().unwrap().push_str(&format!("{}\n", x));
        }),
    );

    RESULT.lock().unwrap().clone()
}

static RESULT: Lazy<Mutex<String>> = Lazy::new(|| Mutex::new(String::new()));
