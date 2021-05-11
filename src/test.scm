(define x 30)
(define y 1000)
(define z (if #t x y))
(display z)

(define-and-run-actor (isEven arg)
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
    (if (even? arg) (string-append (number->string arg) " is even from actor?: Yes") (string-append (number->string arg) " is even from actor?: No")))
)

(define actorId (send-message isEven 50000))
(display actorId)

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
(define a 30)
(display a)
((lambda (x) (define a 70) (display a) (set! a 50) (display a)) 0)
(display a)

(display (letrec ((even?
            (lambda (n)
                (if (= n 0)
                    #t
                    (odd? (- n 1)))))
        (odd?
            (lambda (n)
            (if (= n 0)
                #f
                (even? (- n 1))))))
    (if (even? 780) "607 is even from normal?: Yes" "607 is even from normal?: No!"))
)
(display (await actorId))
