;; A loop as a value, whose inner loop jumps back to it
(define (entry)
  (declare unsafe)
  (cons (let outer ((i 0) (n 0))
          (if (= i 3)
              n
              (let inner ((j 0) (m n))
                (if (= j i)
                    (outer (+ i 1) m)
                    (inner (+ j 1) (+ m 1))))))
        7))
(entry)
---
stdout = "(3 . 7)"
