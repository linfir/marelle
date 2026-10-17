(define (f . xs)
  (let ((add (lambda (x) (set! xs (cons x xs)))))
    (add 1)
    (add 2)
    xs))
(f 3 4)
---
stdout = "(2 1 3 4)"
