(define (add1 x)
  (declare unsafe)
  (+ x 1))

(define (entry)
  (declare unsafe)
  (let ((f add1))
    (f 1 2)))
(entry)
---
stage = "executable"
stderr = "Error: procedure of arity 1 called with 2 arguments\n"
