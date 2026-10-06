(define xs (quote ()))
(define (push n)
  (if (= n 0)
      0
      (begin (set! xs (cons n xs)) (push (- n 1)))))
(begin (push 1000) (fold-left + 0 xs))
---
env = { SCHEME_HEAP = "3000" }
stdout = "500500"
chez = "skip"
