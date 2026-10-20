;; Overlapping copies within one bytevector, both ways
(let ((a (bytevector 1 2 3 4 5))
      (b (bytevector 1 2 3 4 5)))
  (bytevector-copy! a 0 a 2)
  (bytevector-copy! b 2 b 0 3)
  (list a b))
