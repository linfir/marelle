(define x 1)
(define (get) x)
(begin (set! x 7) (get))
