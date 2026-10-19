;; The operands are evaluated left to right
(define n 0)
(define (next) (set! n (+ n 1)) n)
(list (next) (next) (next) (next))
