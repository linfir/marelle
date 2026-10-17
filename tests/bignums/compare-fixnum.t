(define big (+ 1152921504606846975 1))
(define neg (- -1152921504606846976 1))
(list-tail
 (cons 0 (cons (< 5 big) (cons (< big 5) (cons (< neg 5) (cons (> 5 neg)
         (cons (= 5 big) (cons (<= big big) (cons (>= neg big) (cons (= big big) '())))))))))
 1)
---
stdout = "(#t #f #t #t #f #t #f #t)"
