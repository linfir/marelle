(define f 1)
(begin
  (set! f (lambda () 1))
  f)
---
stdout = "#<procedure f>"
