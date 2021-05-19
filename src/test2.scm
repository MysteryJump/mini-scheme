(define (isEven arg) 
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
        (if (even? arg)
            (string-append (number->string arg) " is even from actor?: Yes")
            (string-append (number->string arg) " is even from actor?: No")
        ))
)

(define-and-run-actor (isEvenActor1 arg) (isEven arg))
(define-and-run-actor (isEvenActor2 arg) (isEven arg))
(define-and-run-actor (isEvenActor3 arg) (isEven arg))

(define a1 (send-message isEvenActor1 10000))
(define a2 (send-message isEvenActor2 10001))
(define a3 (send-message isEvenActor3 10002))

(display (string-append "from normal " (isEven 9999)))

(display (string-append "from a1 " (await a1)))
(display (string-append "from a2 " (await a2)))
(display (string-append "from a3 " (await a3)))
(display (string-append "from last " (await (send-message isEvenActor1 5678))))
