(let ((eqv? (lambda (a b) #f)))
  (case (* 10000000000 10000000000) ((100000000000000000000) 'big) (else 'other)))
