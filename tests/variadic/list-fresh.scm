;; Each call builds its own rest list
(define (f . xs) xs)
(eq? (f 1) (f 1))
