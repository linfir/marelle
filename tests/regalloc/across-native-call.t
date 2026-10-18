;; compare-integers is a native: the caller saves its registers around the call
(let ((a 5) (b 6))
  (if (< (expt 2 100) (expt 2 101)) (+ a b) 0))
---
stdout = "11"
