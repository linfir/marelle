;; cdr knows car's pair test: no second test, no error path for cdr
(define (f x) (+ (car x) (cdr x)))
(f (cons 1 2))
