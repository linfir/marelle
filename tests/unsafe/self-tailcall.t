(define (swap n a b)
  (declare unsafe)
  (if (= n 0)
      (cons a b)
      (swap (- n 1) b a)))

(define (entry)
  (declare unsafe)
  (swap 1000001 1 2))
(entry)
---
env = { SCHEME_HEAP = "1000" }
stdout = "(2 . 1)"
gc_stress = "skip"
