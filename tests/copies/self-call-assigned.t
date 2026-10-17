;; A self tail call reads the assigned argument acc
(define (f n acc)
  (if (= n 0)
      acc
      (begin
        (set! acc (+ acc n))
        (f (- n 1) acc))))
(f 10 0)
---
stdout = "55"
