(define p (make-parameter 1))
(list (parameterize ((p 2)) (p 5) (p)) (p))
