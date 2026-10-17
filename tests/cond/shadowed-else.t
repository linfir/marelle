(let ((else #f))
  (cond (else 1) (#t 2)))
---
stdout = "2"
