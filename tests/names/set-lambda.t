(let ((f 1))
  (set! f (lambda () 1))
  f)
---
stdout = "#<procedure f>"
