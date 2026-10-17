(define (fact n)
  (declare unsafe)
  (if (= n 0)
      1
      (* n (fact (- n 1)))))

(define (entry)
  (declare unsafe)
  (fact 10))
(entry)
---
stdout = "3628800"
