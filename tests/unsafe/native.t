;; Raw arguments and results, recursion, and a raw live across a native call
(define (fact (raw n))
  (declare unsafe native (result raw))
  (if (raw= n (raw 0))
      (raw 1)
      (raw* n (fact (raw- n (raw 1))))))

(define (entry)
  (declare unsafe)
  (let ((a (raw 2)))
    (let ((b (fact (raw 5))))
      (as-val (raw+ (raw<< (raw+ a b) (raw 3)) (raw 1))))))
(entry)
---
stdout = "122"
