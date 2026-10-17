(define (even? n)
  (declare unsafe)
  (if (= n 0) #t (odd? (- n 1))))

(define (odd? n)
  (declare unsafe)
  (if (= n 0) #f (even? (- n 1))))

(define (entry)
  (declare unsafe)
  (cons (even? 10) (odd? 7)))
(entry)
---
stdout = "(#t . #t)"
