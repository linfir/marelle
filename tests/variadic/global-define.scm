(define (f . xs) xs)
(define (g a . r) (cons r a))
(cons (f 1 2) (g 3))
