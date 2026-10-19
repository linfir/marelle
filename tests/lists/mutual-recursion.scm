(define (even-length? l) (if (null? l) #t (odd-length? (cdr l))))
(define (odd-length? l) (if (null? l) #f (even-length? (cdr l))))
(even-length? (cons 1 (cons 2 (cons 3 '()))))
