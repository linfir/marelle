(let ((l (iota 1000000)))
  (list (apply + l) (apply < l) (apply < (append l (list 0))) (length (apply list l))))
