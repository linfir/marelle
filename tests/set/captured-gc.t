(define (repeat f i)
  (if (= i 0)
      0
      (begin (f i) (repeat f (- i 1)))))
(let ((xs '())
      (n 0))
  (repeat (lambda (x) (set! xs (cons x xs)) (set! n (+ n 1))) 1000)
  (cons n (fold-left + 0 xs)))
---
env = { SCHEME_HEAP = "3000" }
stdout = "(1000 . 500500)"
