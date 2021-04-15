fn main() {
    mini_scheme::execute(
        &r#"(define-syntax s-cons (syntax-rules () ((_ a b) (cons a (delay b)))))
        (define (s-car s) (car s))
        (define (s-cdr s) (force (cdr s)))
        (define s-null? null?)
        (define (integers-from n) (s-cons n (integers-from (+ n 1))))
        (define integers* (integers-from 1))
    "#,
    )
    .unwrap();
}
