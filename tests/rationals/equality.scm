(list (eqv? 1/2 (/ 2 4)) (eqv? 1/2 1/3) (eqv? 1/2 0) (eq? 2 (* 4 1/2)) (equal? (list 1/2 (vector 2/3)) (list (/ 1 2) (vector (/ 4 6))))
      (member 1/3 (list 1/2 1/3 1/4)) (case (/ 2 6) ((1/2) 'half) ((1/3) 'third) (else 'other)))
