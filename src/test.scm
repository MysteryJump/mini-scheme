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
    (if (even? 7000) (display "Yes") (display "No!"))
)

