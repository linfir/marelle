;; The inner loop jumps back to the outer one
(define (entry)
  (declare unsafe)
  (let outer ((i 0) (n 0))
    (if (= i 3)
        n
        (let inner ((j 0) (m n))
          (if (= j i)
              (outer (+ i 1) m)
              (inner (+ j 1) (+ m 1)))))))
(entry)
---
stdout = "3"
