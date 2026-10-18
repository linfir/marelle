;; A raw loop in a noalloc def
(define (triangle (raw n))
  (declare unsafe noalloc (result raw))
  (let loop ((i n) (acc (raw 0)))
    (if (raw= i (raw 0))
        acc
        (loop (raw- i (raw 1)) (raw+ acc i)))))

(define (entry)
  (declare unsafe)
  (as-val (raw+ (raw<< (triangle (raw 100)) (raw 3)) (raw 1))))
(entry)
---
stdout = "5050"
