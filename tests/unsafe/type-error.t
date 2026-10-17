(define (entry)
  (declare unsafe)
  (car 5))
(entry)
---
stage = "executable"
stderr = "Error: `car` expected a pair: 5\n"
