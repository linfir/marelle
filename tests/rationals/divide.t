(define (call f . args) (apply f args))
(list (/ 2) (/ -1/3) (/ 60 2 3) (/ 1 2 3) (call / 1 4) (call / 5) (apply / (list 100 10 20)))
---
stdout = "(1/2 -3 10 1/6 1/4 1/5 1/2)"
