;; A literal is the same object at each evaluation
(define (f) '(1 2))
(eq? (f) (f))
