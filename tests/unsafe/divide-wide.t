;; (2^64 - 2) * 2^64 + 1 = (2^64 - 1)^2, and 2^64 / 3
(define (entry)
  (declare unsafe)
  (cons (raw= (raw/wu (raw -2) (raw 1) (raw -1)) (raw -1))
        (as-val (raw+ (raw<< (raw/wu (raw 1) (raw 0) (raw 12297829382473034411)) (raw 3))
                      (raw 1)))))
(entry)
---
stdout = "(#t . 1)"
