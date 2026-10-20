(define big (+ 1152921504606846975 1))
(define neg (- -1152921504606846976 1))
(list (< 5 big) (< big 5) (< neg 5) (> 5 neg)
      (= 5 big) (<= big big) (>= neg big) (= big big))
