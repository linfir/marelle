(let ((f car))
  (set! f cdr)
  (f (cons 1 2)))
---
stdout = "2"
