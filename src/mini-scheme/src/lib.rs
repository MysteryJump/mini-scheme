use lexer::TokenKind;

mod ast;
mod lexer;
mod parser;

pub fn execute(source: &str) -> Result<String, String> {
    let lexed = lexer::lex(source);
    println!(
        "{:#?}",
        lexed
            .iter()
            .map(|x| x.kind)
            .filter(|x| !(x == &TokenKind::Other))
            .collect::<Vec<_>>()
    );
    Ok("".to_string())
}
