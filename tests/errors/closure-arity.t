(define (make-adder n) (lambda (x) (+ x n)))
((make-adder 1))
---
stage = "executable"
stderr_contains = "procedure of arity 1 called with 0 arguments"
