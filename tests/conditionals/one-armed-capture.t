(let ((x 5))
  ((lambda () (if #t x))))
---
stdout = "5"
