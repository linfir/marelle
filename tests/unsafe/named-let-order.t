;; The inits are evaluated left to right
(define (show x)
  (declare unsafe)
  (display stdout x)
  x)

(define (entry)
  (declare unsafe)
  (let loop ((a (show 1)) (b (show 2)) (c (show 3)) (d (show 4)) (e (show 5)))
    (put-string stdout " ")
    a))
(entry)
---
stdout = "12345 1"
