;; Literal symbols from different parts of the program are the same symbol
(define x 'shared)
(define (f) 'shared)
(define (g) (let ((h (lambda () 'shared))) (h)))
(list (eq? x (f)) (eq? (f) (g)) (eq? (g) (string->symbol "shared")))
