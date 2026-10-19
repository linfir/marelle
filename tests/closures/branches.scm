(define (choose b x) (if b (lambda () x) (lambda () (- 0 x))))
(cons ((choose #t 5)) ((choose #f 5)))
