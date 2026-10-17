(let ((n 0))
  (case (begin (set! n (+ n 1)) n) ((5) 0) ((6) 0) (else n)))
---
stdout = "1"
