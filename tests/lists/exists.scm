(list (exists even? '(1 4 5)) (exists even? '(1 3)) (exists even? '())
      (exists (lambda (x) (and (> x 1) (* x 10))) '(1 2 3))
      (exists = '(1 2) '(3 2)) (exists = '(1 2) '(3 4)))
