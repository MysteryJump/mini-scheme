(define x 30)
(define y 1000)
(define z (if #t x y))
(display z)

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
    (display "9999 is even?")
    (if (even? 9999) (display "Yes") (display "No!"))
)

(display (let loop ((numbers '(3 -2 1 6 -5)) (nonneg '()) (neg '()))
    (cond 
        ((null? numbers) (list nonneg neg))
        ((>= (car numbers) 0)
            (loop (cdr numbers)
            (cons (car numbers) nonneg)
            neg))
            ((< (car numbers) 0)
            (loop (cdr numbers)
        nonneg
    (cons (car numbers) neg))))))

(display (let ((x '(1 3 5 7 9)))
    (do ((x x (cdr x))
        (sum 0 (+ sum (car x))))

        ((null? x) sum) 0
    )
))
(display 30)