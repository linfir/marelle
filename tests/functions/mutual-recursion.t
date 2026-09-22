(define (even n)
  (if (= n 0)
      1
      (odd (- n 1))))

(define (odd n)
  (if (= n 0)
      0
      (even (- n 1))))

(even 10)
---
stdout = "1"
