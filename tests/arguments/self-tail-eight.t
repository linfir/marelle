(define (loop n a b c d e f acc)
  (if (= n 0)
      (cons acc (cons a f))
      (loop (- n 1) f a b c d e (+ acc 1))))
(loop 1000000 1 2 3 4 5 6 0)
---
stdout = "(1000000 3 . 2)"
