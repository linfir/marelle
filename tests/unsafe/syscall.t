;; write(1, "hi ", 3)
(define (entry)
  (declare unsafe)
  (syscall 1 (raw 1) "hi " (raw 3))
  0)
(entry)
---
stdout = "hi 0"
