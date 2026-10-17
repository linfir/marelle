(letrec ((n 0) (bump (lambda () (set! n (+ n 1)) n))) (bump) (bump))
---
stdout = "2"
