(define (entry)
  (declare unsafe)
  (5 1))
(entry)
---
stage = "executable"
stderr = "Error: expected procedure: 5\n"
