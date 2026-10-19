(list (procedure? car) (procedure? (lambda (x) x)) (procedure? (let ((n 1)) (lambda () n))) (procedure? 1) (procedure? (cons 1 2)) (procedure? (vector)))
