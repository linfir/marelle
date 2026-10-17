;; a-b and a_b both give the symbol a_b_code; natives keep their names
(define (a-b x)
  (declare unsafe native)
  x)

(define (a_b x)
  (declare unsafe native)
  x)

(define (entry)
  (declare unsafe)
  (a-b (a_b 1)))
(entry)
---
stage = "compiler"
stderr_contains = "symbol clash"
