use std::env;

#[rustfmt::skip]
const HELP_TEXT: &str = 
r#"Usage: mins [options] [file]
Options:
    --repl, -r  REPL mode"#;

fn main() {
    let args = env::args().skip(1).collect::<Vec<_>>();
    let mut is_repl = false;
    let mut is_help = false;
    let mut is_debug = false;
    let mut path = None;
    for item in args {
        if item == "--repl" || item == "-r" {
            is_repl = true;
        } else if item == "--help" || item == "-h" {
            is_help = true;
        } else if item == "--debug" {
            is_debug = true;
        } else {
            path = Some(item);
        }
    }

    if is_repl {
        repl();
    } else if is_help {
        println!("{}", HELP_TEXT);
    } else if is_debug {
        do_debug();
    } else if let Some(path) = path {
        mini_scheme::execute(&std::fs::read_to_string(path).unwrap());
    } else {
        println!("{}", HELP_TEXT);
    }
}

fn repl() {
    // mini_scheme::execute(source.to_string())
}

fn do_debug() {
    mini_scheme::execute(
        &r#"
        (define a 50)
        (define (t y) (define a 30) (if y 3 a))
          (t #f)
          a
          (define (sum x) (if (> x 0) (- x 1) 5))
          (sum 3)
          (define (fact x n) (if (> n 0) (* x (fact x (- n 1))) 1))
          (fact 3 2)
          (define (fib n)
              (if (<= n 2)
              1
              (+ (fib (- n 1)) (fib (- n 2))))
          )
          (define nn (fib 10))
          nn
          (set! nn 1000)
          nn
          (let fact33 ((x 7) (y 3))
                (if (= x 0) 1
                (* y (fact33 (- x 1) y))))
          (let* ((x 3) (y 4) (z y)) (+ z x))
          (string->symbol "33")
          (let ((x 2) (y 3))
              (let ((x 7)
                  (z (+ x y)))
                  (* z x)))
          (let ((x 2) (y 3))
              (let* ((x 7)
                  (z (+ x y)))
                  (* z x)))
          (letrec ((even?
                  (lambda (n)
                      (if (= n 0)
                      #t
                      (odd? (- n 1)))))
              (odd?
                  (lambda (n)
                      (if (= n 0)
                      #f
                      (even? (- n 1))))))
              (even? 88))
          (define cc '(3 (3) (3 #t) . (3 . name)))
          (car cc)
          (car (cdr (cdr cc)))
          (define lss '(2 2 3 3 (3 . 5)))
          (length lss)
          (memq lss 3)
          (last '(3 . ()))
          (append '(3 9 (3 . 3)) '(3))
          (set-car! lss 5)
          lss
          (set-cdr! lss '(3))
          lss
          (define x "aa")
          (define y (string-append x "a"))
          (eq? y "aaa")
          (define (Hack x y . z) z)
          (display (Hack 1 2 3 4 5))
        "#,
        // TODO: Add some test
    )
    .iter()
    .for_each(|x| match x {
        Ok(_) => {}
        Err(e) => eprintln!("{}", e),
    });
}
