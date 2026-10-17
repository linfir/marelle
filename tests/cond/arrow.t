(cond ((< 2 1) => (lambda (x) 0))
      ((car (cons 41 0)) => (lambda (x) (+ x 1)))
      (else 2))
---
stdout = "42"
