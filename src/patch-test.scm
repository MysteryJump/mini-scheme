(define (assert-shallow left right) 
    (if (eq? left right) '() (begin (display "Assertion Error: left,right is") (display left) (display right) (display "")))
)
(define (assert-deep left right)
    (if (equal? left right) '() (begin (display "Assertion Error: left,right is") (display left) (display right) (display "")))
)

(define x 28)
(assert-shallow x 28)

(assert-shallow (procedure? (lambda (x) (+ x x))) #t)
(assert-shallow ((lambda (x) (+ x x)) 4) 8)

(define reverse-subtract
    (lambda (x y) (- y x)))
(assert-shallow (reverse-subtract 7 10) 3)

(define add4
    (let ((x 4))
    (lambda (y) (+ x y))))
(assert-shallow (add4 6) 10)
(assert-shallow x 28)

(assert-deep ((lambda x x) '(3 4 5 6)) '(3 4 5 6))
(assert-deep ((lambda (x y . z) z) 3 4 5 6) '(5 6))

(assert-shallow (if (> 3 2) 'yes 'no) 'yes)
(assert-shallow (if (> 2 3) 'yes 'no) 'no)
(assert-shallow (if (> 3 2)
    (- 3 2)
    (+ 3 2)) 1)

(define x 2)
(assert-shallow (+ x 1) 3)
(set! x 4)
(assert-shallow (+ x 1) 5)


(assert-shallow (cond ((> 3 2) 'greater)
    ((< 3 2) 'less)) 'greater)
(assert-shallow (cond ((> 3 3) 'greater)
    ((< 3 3) 'less)
    (else 'equal)) 'equal)

(assert-shallow (and (= 2 2) (> 2 1)) #t)
(assert-shallow (and (= 2 2) (< 2 1)) #f)
(assert-deep (and 1 2 'c '(f g)) '(f g))
(assert-shallow (and) #t)

(assert-shallow (or (= 2 2) (> 2 1)) #t)
(assert-shallow (or (= 2 2) (< 2 1)) #t)
(assert-shallow (or #f #f #f) #f)
(assert-deep (or (memq 'b '(a b c)) (/ 3 0)) '(b c))

(assert-shallow (let ((x 2) (y 3)) (* x y)) 6)
(assert-shallow (let ((x 2) (y 3)) (let ((x 7) (z (+ x y))) (* z x))) 35)
(assert-shallow (let ((x 2) (y 3)) (let* ((x 7) (z (+ x y))) (* z x))) 70)

(assert-shallow (letrec (
    (even?
        (lambda (n)
            (if (= 0 n)
            #t
            (odd? (- n 1)))))
    (odd?
        (lambda (n)
            (if (= 0 n)
            #f
            (even? (- n 1))))))
    (even? 88)) #t
)

(define x 0)

(assert-shallow (and (= x 0)
    (begin (set! x 5)
    (+ x 1))) 6)

(assert-shallow 
    (let ((x '(1 3 5 7 9)))
        (do ((x x (cdr x))
        (sum 0 (+ sum (car x))))
        ((null? x) sum) 0))
    25
    )

(assert-deep (let loop ((numbers '(3 -2 1 6 -5))
                    (nonneg '())
                    (neg '()))
    (cond ((null? numbers) (list nonneg neg))
        ((>= (car numbers) 0)
        (loop (cdr numbers)
            (cons (car numbers) nonneg)
            neg))
        ((< (car numbers) 0)
        (loop (cdr numbers)
            nonneg
            (cons (car numbers) neg)))))
    '((6 1 3) (-5 -2))
)

(define add3
    (lambda (x) (+ x 3)))
(assert-shallow (add3 3) 6)
(define first car)
(assert-shallow (first '(1 2)) 1)

(assert-shallow (let ((x 5))
    (define foo (lambda (y) (bar x y)))
    (define bar (lambda (a b) (+ (* a b) a)))
    (foo (+ x 3))) 45)

(assert-shallow (eq? 'a 'a) #t)
(assert-shallow (eq? 'a 'b) #f)
(assert-shallow (eq? 2 2) #t)
(assert-shallow (eq? (list 'a) (list 'a')) #f)
(assert-shallow (eq? '() '()) #t)
(assert-shallow (eq? car car) #t)
(assert-shallow (let ((x '(a)))
    (eq? x x)) #t)
(assert-shallow (let ((p (lambda (x) x)))
    (eq? p p)) #t)

(assert-shallow (equal? 'a 'a) #t)
(assert-shallow (equal? '(a (b) c) '(a (b) c)) #t)
(assert-shallow (equal? "abc" "abc") #t)

(assert-shallow (number? (/ 6 10)) #t)
(assert-shallow (number? (/ 6 3)) #t)
(assert-shallow (integer? (/ 8 4)) #t)
(assert-shallow (integer? (/ 6 5)) #f)

(assert-deep (number->string 3) "3")
(assert-deep (number->string (/ 1 3)) "1/3")
(assert-deep (string->number "3") 3)

(assert-shallow (not #t) #f)
(assert-shallow (not 3) #f)
(assert-shallow (not (list 3)) #f)
(assert-shallow (not #f) #t)
(assert-shallow (not '()) #f)
(assert-shallow (not (list)) #f)
(assert-shallow (not 'nil) #f)

(assert-shallow (boolean? #f) #t)
(assert-shallow (boolean? 0) #f)
(assert-shallow (boolean? '()) #f)

;(define x (list 'a 'b 'c))
;(define y x)
;(assert-deep y '(a b c))
;(assert-shallow (list? y) #t)
;(set-cdr! x 4)
;(assert-deep x '(a . 4))
;(assert-shallow (eq? x y) #t)
;(set-cdr! x x)
;(assert-shallow (list? x) #f)
