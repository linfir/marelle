;; raw/ and raw% truncate toward zero
(define (int-value (raw n))
  (declare unsafe inline)
  (as-val (raw+ (raw<< n (raw 3)) (raw 1))))

(define (entry)
  (declare unsafe)
  (cons (int-value (raw/ (raw -7) (raw 2)))
        (cons (int-value (raw% (raw -7) (raw 2)))
              (cons (int-value (raw/ (raw 7) (raw -2)))
                    '()))))
(entry)
---
stdout = "(-3 -1 -3)"
