;; Each iteration gets its own box
(let loop ((i 0) (acc '()))
  (if (= i 3)
      (map (lambda (f) (f)) acc)
      (loop (+ i 1) (cons (lambda () (set! i (* i 10)) i) acc))))
---
stdout = "(20 10 0)"
