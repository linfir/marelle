;; compare-integers is a native, which keeps its callee-saved registers
(let ((a 5) (b 6))
  (if (< (expt 2 100) (expt 2 101)) (+ a b) 0))
---
stdout = "11"
