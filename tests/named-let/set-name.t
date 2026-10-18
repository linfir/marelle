(let loop ((i 0))
  (if (= i 0)
      (begin (set! loop (lambda (j) (* j 10))) (loop 4))
      i))
---
stdout = "40"
