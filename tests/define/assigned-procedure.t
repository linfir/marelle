(define (f) 1)
(set! f (lambda () 2))
(f)
---
stdout = "2"
