(define-properties ((a 30) (b 80) (e 90)) 
    (
        (c (+ a b e))
        (d ((lambda (arg) (+ arg 90)) e))
    ))

(display a)
(display b)
(display e)
(display c)
(display d)
(set! a 50)
(display "")
(display a)
(display b)
(display e)
(display c)
(display d)
(set! a -50)
(display "")
(display a)
(display b)
(display e)
(display c)
(display d)
(set! e 80)
(display "")
(display a)
(display b)
(display e)
(display c)
(display d)