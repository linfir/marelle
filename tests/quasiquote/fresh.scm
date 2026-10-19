;; Unquoted parts make new pairs at each evaluation
(define (f x) `(,x))
(eq? (f 1) (f 1))
