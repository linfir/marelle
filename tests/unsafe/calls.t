(define (pair-sum p)
  (declare unsafe)
  (+ (car p) (cdr p)))

(define (wrap x)
  (declare unsafe)
  (cons x (cons x '())))

(define (entry)
  (declare unsafe)
  (cons (pair-sum (cons 40 2)) (wrap (wrap 7))))
(entry)
---
stdout = "(42 (7 7) (7 7))"
env = { SCHEME_GC_STRESS = "1" }
