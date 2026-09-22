(define (fact n)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))
(fact 5)
---
stdout = "120"
