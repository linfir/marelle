(define (len l) (if (null? l) 0 (+ 1 (len (cdr l)))))
(len (cons 1 (cons 2 (cons 3 '()))))
