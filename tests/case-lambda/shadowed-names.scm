;; The expansion uses the library length, car and cdr
(let ((length 0) (car 1) (cdr 2))
  ((case-lambda ((x y) (list x y length car cdr))) 3 4))
