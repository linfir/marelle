(define (k x) (let ((c (lambda () x))) c))
(let ((p (k 7)))
  (cons p (cons (p) (quote ()))))
---
stdout = "(#<procedure c> 7)"
env = { SCHEME_GC_STRESS = "1" }
