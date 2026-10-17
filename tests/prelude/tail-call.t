;; Known tail calls into the prelude, which is in lib.o
(define (count-down n l)
  (if (= n 0)
      (length l)
      (count-down (- n 1) (cons n l))))
(define (last-of l) (reverse l))
(cons (count-down 100000 '()) (last-of (cons 1 (cons 2 '()))))
---
stdout = "(100000 2 1)"
