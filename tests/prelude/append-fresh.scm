(let* ((a (list 1 2)) (r (append a (list 3))))
  (list r (eq? r a) (eq? (cdr r) (cdr a)) a))
