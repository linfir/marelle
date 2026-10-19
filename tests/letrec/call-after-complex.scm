;; A simple init calling a primitive may fail, so it stays after the complex ones
(letrec* ((a (vector-ref (make-vector 1 0) 5))
          (b (car 1)))
  b)
