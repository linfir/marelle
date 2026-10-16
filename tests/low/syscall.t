;; write(1, "hi ", 3)
(def main () -> (val) ()
     (syscall 1 (raw 1) "hi " (raw 3))
     0)
---
stdout = "hi 0"
