((lambda (loop n) (loop loop n 0))
 (lambda (self n acc) (if (= n 0) acc (self self (- n 1) (+ acc 1))))
 1000000)
---
stdout = "1000000"
