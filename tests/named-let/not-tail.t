(let loop ((i 0)) (if (= i 5) 0 (+ 1 (loop (+ i 1)))))
---
stdout = "5"
