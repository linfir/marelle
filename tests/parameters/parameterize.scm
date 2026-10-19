(define p (make-parameter 1))
(list (p) (parameterize ((p 2)) (p)) (p))
