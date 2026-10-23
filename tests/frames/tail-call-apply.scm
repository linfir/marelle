;; apply in tail position: its frame moved down with a count known only at run time
(define (step n acc)
  (if (= n 0) acc (apply step (list (- n 1) (+ acc 1)))))
(step 10000000 0)
