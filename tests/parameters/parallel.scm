(define p (make-parameter 1))
(define q (make-parameter 10))
(list (parameterize ((p 2) (q (p))) (list (p) (q))) (p) (q))
