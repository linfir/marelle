(define p (make-parameter 1))
(parameterize ((p 2))
  (define (double) (* 2 (p)))
  (double))
