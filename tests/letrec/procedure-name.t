(letrec ((fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1))))))) (cons fact (fact 10)))
---
stdout = "(#<procedure fact> . 3628800)"
