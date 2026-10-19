(define (f) 1)
(begin (set! f (lambda () 2)) (f))
