;; A loop as a value, exiting through if, let and begin
(define (entry)
  (declare unsafe)
  (cons (let loop ((i 0))
          (if (= i 3)
              (let ((j (+ i 1)))
                (begin j))
              (loop (+ i 1))))
        7))
(entry)
---
stdout = "(4 . 7)"
