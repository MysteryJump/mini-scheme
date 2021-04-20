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
        (define nn (fib 10))
        nn
        (set! nn 1000)
        nn
        (let fact33 ((x 7) (y 3))
              (if (= x 0) 1
              (* y (fact33 (- x 1) y))))
        (let* ((x 3) (y 4) (z y)) (+ z x))
        "#, // TODO: Add some test
    )
    .unwrap();
}
