#![feature(bool_to_option)]

use interpreter::Interpreter;
use parser::Parser;

mod ast;
mod interpreter;
mod lexer;
mod parser;

pub fn execute(source: &str) -> Result<String, String> {
    let lexed = lexer::lex(source);
    // println!(
    //     "{:#?}",
    //     lexed
    //         .iter()
    //         .map(|x| x.kind)
    //         .filter(|x| !(x == &TokenKind::Other))
    //         .collect::<Vec<_>>()
    // );
    let parser = Parser::new(&lexed);
    let interpreter = Interpreter::new();
    while let Some(pp) = parser.parse_toplevel() {
        // println!("{:#?}", pp);
        match interpreter.execute_toplevel(pp) {
            Ok(result) => {
                println!("{}", result)
            }
            Err(e) => println!("{}", e),
        }
    }
    Ok("".to_string())
}
