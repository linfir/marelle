(define (make k)
  (lambda (a b c d e f g h) (+ k (+ a h))))
((make 100) 1 2 3 4 5 6 7 8)
