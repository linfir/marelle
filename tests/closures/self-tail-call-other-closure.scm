(define (make k)
  (lambda (n acc)
    (if (= n 0)
        acc
        ((make (+ k 1)) (- n 1) (+ acc k)))))
((make 1) 4 0)
