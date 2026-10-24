(let* ((l (list 1 2 3))
       (m (apply list l))
       (n (apply list 0 l)))
  (list (eq? l m) (eq? (cdr l) (cdr m)) (eq? l (cdr n)) m n))
