;; An inlined callee's errors keep their wording
(define (first x) (car x))
(define (g y) (first y))
(g 5)
