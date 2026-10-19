(list (for-all even? '(2 4)) (for-all even? '(2 3)) (for-all even? '())
      (for-all (lambda (x) (+ x 1)) '(1 2))
      (for-all < '(1 2) '(2 3)) (for-all < '(1 3) '(2 3)))
