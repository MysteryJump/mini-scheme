fn main() {
    mini_scheme::execute(
        &r#"(define a 50)
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
        
        "#,
        // TODO: Add some test
    )
    .unwrap();
}
