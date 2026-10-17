(define (entry)
  (declare unsafe)
  (let loop ((i 0))
    (+ 1 (loop i))))
(entry)
---
stage = "compiler"
stderr_contains = "called not in exit position"
