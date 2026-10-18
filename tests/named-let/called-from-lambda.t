(let loop ((i 0)) (if (= i 3) i ((lambda () (loop (+ i 1))))))
---
stdout = "3"
