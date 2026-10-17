;; Comparisons in test position, with a bignum operand
(define (classify n) (if (< n 0) -1 (if (> n 1152921504606846975) 2 1)))
(cons (classify (- -1152921504606846976 1)) (cons (classify 7) (classify (+ 1152921504606846975 7))))
---
stdout = "(-1 1 . 2)"
