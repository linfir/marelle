;; A literal is the same vector at each evaluation
(define (f) '#(0))
(eq? (f) (f))
