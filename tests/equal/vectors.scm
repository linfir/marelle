(list (equal? (vector 1 (list 2) "c") (vector 1 (list 2) "c")) (equal? (vector 1 2) (vector 1 2 3))
      (equal? (vector) (vector)) (equal? (vector 1 2) (list 1 2)) (equal? (vector (vector 1)) (vector (vector 2))))
