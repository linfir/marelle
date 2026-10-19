(define p (make-parameter 1))
(call-with-values (lambda () (parameterize ((p 2)) (values (p) 3))) list)
