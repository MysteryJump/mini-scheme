#![feature(async_closure)]

use std::{
    env,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc, Mutex,
    },
};
use std::{io::Write, process::exit};

use mini_scheme::Repl;

#[rustfmt::skip]
const HELP_TEXT: &str = 
r#"Usage: mins [options] [file]
Options:
    --repl, -r  REPL mode"#;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
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
        repl().await;
    } else if is_help {
        println!("{}", HELP_TEXT);
    } else if is_debug {
        do_debug();
    } else if let Some(path) = path {
        mini_scheme::execute(&std::fs::read_to_string(path).unwrap());
    } else {
        println!("{}", HELP_TEXT);
    }
    Ok(())
}

async fn repl() {
    let mut repl = Repl::new();
    // let (tx2, rx2) = tokio::sync::mpsc::channel(2);
    // let rx2 = Arc::new(tokio::sync::Mutex::new(rx2));

    // let call_exit = Arc::new(AtomicBool::new(true));
    // let in_handler = call_exit.clone();

    // ctrlc::set_handler(move || {
    //     if in_handler.load(Ordering::SeqCst) {
    //         exit(0);
    //     } else {
    //         #[allow(unused_must_use)]
    //         tx2.send(());
    //     }
    // })
    // .unwrap();

    loop {
        let (tx1, rx1) = tokio::sync::oneshot::channel();
        // let rx2 = rx2.clone();

        let mut line = String::new();
        print!("> ");
        std::io::stdout().flush().unwrap();
        loop {
            std::io::stdin().read_line(&mut line).unwrap();
            let paren_depth = line.chars().filter(|x| x == &'(').count()
                - line.chars().filter(|x| x == &')').count();
            if paren_depth == 0 {
                break;
            }
        }
        if line.trim_end() == "#exit" {
            break;
        }

        let before_repl = repl.clone();
        // call_exit.store(false, Ordering::SeqCst);

        let mut stream =
            tokio::signal::unix::signal(tokio::signal::unix::SignalKind::interrupt()).unwrap();

        tokio::spawn(async move {
            tokio::select! {
                _ = stream.recv() => {
                    println!("Ctrl+C");
                    let _ = tx1.send(None);
                }
                val = call_repl(repl, line) => {
                    let _ = tx1.send(Some(val));
                }

            }
        });

        let res = rx1.await.unwrap();

        if let Some(r) = res {
            repl = r;
        } else {
            repl = before_repl;
        }

        unsafe {
            signal_hook_registry::register(signal_hook::consts::SIGINT, || {
                println!();
                exit(0);
            })
        }
        .unwrap();

        // call_exit.store(true, Ordering::SeqCst);

        // repl = std::thread::spawn(move || {
        //     if line.trim_end() == "#cenv" {
        //         println!("{:#?}", repl.get_current_env());
        //     } else {
        //         let result = repl.execute_line(&line);
        //         if !result.is_empty() {
        //             println!("{}", result);
        //         }
        //     }
        //     repl
        // })
        // .join()
        // .unwrap();
    }
}

async fn call_repl(mut repl: Repl, line: String) -> Repl {
    if line.trim_end() == "#cenv" {
        println!("{:#?}", repl.get_current_env());
    } else {
        let result = repl.execute_line(&line);
        if !result.is_empty() {
            println!("{}", result);
        }
    }
    repl
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
              (even? 548))
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
        // TODO: parser error
        // TODO: lexer negative number
    )
    .iter()
    .for_each(|x| match x {
        Ok(_) => {}
        Err(e) => eprintln!("{}", e),
    });
}
