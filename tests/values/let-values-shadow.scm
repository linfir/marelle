(let ((call-with-values 0) (list 1) (car 2) (lambda 3))
  (let-values (((x y) (values 1 2)) ((z) 3))
    (+ x y z)))
