;; f is applied left to right
(define seen '())
(define r (map (lambda (x) (set! seen (cons x seen)) (* x 10)) (list 1 2 3)))
(list r seen)
---
stdout = "((10 20 30) (3 2 1))"
chez = "skip"
