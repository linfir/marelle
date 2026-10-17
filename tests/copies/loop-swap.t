;; Each argument is read before any loop variable is set
(let loop ((a 1) (b 2) (n 3))
  (if (= n 0)
      (list a b)
      (loop b a (- n 1))))
---
stdout = "(2 1)"
