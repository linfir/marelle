(define (f x) (if (cond ((< x 0) #f) ((< x 10) #t) (else #f)) 1 2))
(cons (f -1) (cons (f 5) (f 20)))
