fn main() {
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
        (fib 10)
        "#, // TODO: Add some test
    )
    .unwrap();
}
