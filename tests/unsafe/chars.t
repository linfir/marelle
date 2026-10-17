;; Character literals are immediates
(define (entry)
  (declare unsafe)
  (if (raw= (as-raw #\a) (raw 24866)) ; (97 << 8) | 34
      #\b
      #\c))
(entry)
---
stdout = "#\\b"
