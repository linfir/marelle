(define (rev l acc) (if (null? l) acc (rev (cdr l) (cons (car l) acc))))
(rev (cons 1 (cons 2 (cons 3 '()))) '())
