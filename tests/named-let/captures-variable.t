(define (f n)
  (let loop ((i 0))
    (if (= i n) (list i) (cons i (loop (+ i 1))))))
(f 3)
---
stdout = "(0 1 2 3)"
