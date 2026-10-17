(define (f n)
  (let ((k 7))
    (let loop ((i 0) (acc 0))
      (if (= i n) (+ acc k) (loop (+ i 1) (+ acc i))))))
(f 100)
---
stdout = "4957"
