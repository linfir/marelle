(call-with-values (lambda () (values 1 2 3)) (lambda (a . r) (list a r)))
