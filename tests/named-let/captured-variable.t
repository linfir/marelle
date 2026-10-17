(let loop ((i 0) (fs '()))
  (if (= i 3)
      (map (lambda (f) (f)) fs)
      (loop (+ i 1) (cons (lambda () i) fs))))
---
stdout = "(2 1 0)"
