(define (make-counter)
  (let ((n 0))
    (lambda () (set! n (+ n 1)) n)))
(let ((a (make-counter))
      (b (make-counter)))
  (a)
  (a)
  (b)
  (cons (a) (b)))
---
stdout = "(3 . 2)"
