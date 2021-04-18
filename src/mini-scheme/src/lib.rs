#![feature(bool_to_option)]

use parser::Parser;

mod ast;
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
    let mut parser = Parser::new(&lexed);
    while let Some(pp) = parser.parse_toplevel() {
        println!("{:#?}", pp);
    }
    Ok("".to_string())
}
