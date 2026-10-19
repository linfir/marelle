(define f
  (case-lambda
    (() 0)
    ((x) (list 1 x))
    ((x y) (list 2 x y))
    ((x . r) (list (quote many) x r))))
(list (f) (f 1) (f 1 2) (f 1 2 3))
