use std::str::Chars;

#[derive(Debug, Clone)]
pub struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenKind<'a> {
    OpenParen,
    CloseParen,
    Dot,
    Str(&'a str),   // e.g. "Hello, world!"
    Ident(&'a str), // e.g. define
    Quote,
    True,
    False,
    Num(i64),
    Other,   // such as whitespace, control char
    Unknown, // cannot lexing token
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub ln: i32,
    pub col: i32,
    pub abs: i32,
    pub len: i32, // TODO: support utf-8 multi-byte char?
}

impl Span {
    fn advance_char(self) -> Span {
        Span {
            ln: self.ln,
            col: { self.col + 1 },
            abs: { self.abs + 1 },
            len: 1,
        }
    }
}

pub fn lex(src: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut source = src;
    let mut bs = Span {
        ln: 1,
        col: 0,
        abs: 0,
        len: 0,
    };
    while let Some(nt) = next_token(source, bs) {
        bs = nt.span;
        source = &source[bs.len as usize..];
        tokens.push(nt);
    }
    tokens
}

fn next_token(src: &str, before_span: Span) -> Option<Token> {
    let mut chars = src.chars();
    let next = chars.next()?;
    Some(match next {
        '(' => Token {
            kind: TokenKind::OpenParen,
            span: before_span.advance_char(),
        },
        ')' => Token {
            kind: TokenKind::CloseParen,
            span: before_span.advance_char(),
        },
        ' ' | '\t' | '\r' => {
            before_span.advance_char();
            Token {
                kind: TokenKind::Other,
                span: before_span.advance_char(),
            }
        }
        '\n' => Token {
            kind: TokenKind::Other,
            span: Span {
                ln: before_span.ln + 1,
                col: 0,
                abs: before_span.abs + 1,
                len: 1,
            },
        },
        '\'' => Token {
            kind: TokenKind::Quote,
            span: Span {
                ln: before_span.ln + 1,
                abs: before_span.abs + 1,
                ..before_span
            },
        },
        '#' => boolean(&mut chars, before_span),
        '"' => string(&mut chars, src, before_span),
        '0'..='9'
        | 'a'..='z'
        | 'A'..='Z'
        | '!'
        | '$'
        | '%'
        | '&'
        | '*'
        | '+'
        | '-'
        | '.'
        | '/'
        | '<'
        | '='
        | '>'
        | '?'
        | '@'
        | '^'
        | '_' => ident_or_num(&mut chars, src, before_span),
        _ => panic!(),
    })
}

fn boolean<'a>(chars: &mut Chars, bf: Span) -> Token<'a> {
    match chars.next() {
        Some('t') => Token {
            kind: TokenKind::True,
            span: Span {
                len: 2,
                abs: bf.ln + 2,
                ..bf
            },
        },
        Some('f') => Token {
            kind: TokenKind::False,
            span: Span {
                len: 2,
                abs: bf.ln + 2,
                ..bf
            },
        },
        Some(s) => {
            let mut chs = read_to_termination(chars);
            chs.insert(0, '#');
            chs.insert(1, s);
            Token {
                kind: TokenKind::Unknown,
                span: Span {
                    len: chs.len() as i32,
                    abs: bf.abs + chs.len() as i32,
                    col: bf.col + chs.len() as i32,
                    ..bf
                },
            }
        }
        None => Token {
            kind: TokenKind::Unknown,
            span: bf.advance_char(),
        },
    }
}

fn string<'a>(chars: &mut Chars, src: &'a str, bf: Span) -> Token<'a> {
    let mut ignore_next = false;
    let mut last_ind = 0;
    for (ind, ch) in chars.enumerate() {
        match ch {
            '"' if ignore_next => {
                ignore_next = false;
            }
            '"' => {
                last_ind = ind;
                break;
            }
            '\\' => {
                ignore_next = true;
            }
            '\n' => {
                // TODO: as unknown token
                panic!();
            }
            _ => {}
        }
    }
    let len = (last_ind + 2) as i32;
    Token {
        kind: TokenKind::Str(&src[1..=last_ind]),
        span: Span {
            col: bf.col + 1,
            ln: bf.ln,
            len,
            abs: bf.abs + 1,
        },
    }
}

fn ident_or_num<'a>(chars: &mut Chars, src: &'a str, bf: Span) -> Token<'a> {
    let mut ch = read_to_termination(chars);
    let f = src.chars().next().unwrap();
    if f == '.' && ch.is_empty() {
        Token {
            kind: TokenKind::Dot,
            span: Span {
                len: 1,
                col: bf.col + 1,
                ..bf
            },
        }
    } else if (f.is_ascii_digit() || (f == '-' && !ch.is_empty()))
        && ch.iter().all(|x| x.is_ascii_digit())
    {
        ch.insert(0, f);
        let len = ch.len() as i32;
        Token {
            kind: TokenKind::Num(ch.iter().collect::<String>().parse().unwrap()),
            span: Span {
                len,
                col: bf.col + len,
                ..bf
            },
        }
    } else {
        let len = ch.len() as i32 + 1;
        Token {
            kind: TokenKind::Ident(&src[0..ch.len() + 1]),
            span: Span {
                len,
                col: bf.col + len,
                ..bf
            },
        }
    }
}

fn read_to_termination(chars: &mut Chars) -> Vec<char> {
    let mut chs = Vec::new();
    for s in chars {
        if s == '(' || s == ')' || s == ' ' || s == '\n' || s == '\r' {
            break;
        } else {
            chs.push(s);
        }
    }
    chs
}
