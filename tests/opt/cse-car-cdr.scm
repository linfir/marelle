;; cdr reuses car's pair check and untagging
(define (f x) (+ (car x) (cdr x)))
(f (cons 1 2))
