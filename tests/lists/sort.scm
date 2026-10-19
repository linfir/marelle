(define pairs '((3 . a) (1 . b) (3 . c) (2 . d) (1 . e)))
(list (sort < '(5 2 9 1 5 3)) (list-sort < '()) (sort < '(1))
      (list-sort (lambda (x y) (< (car x) (car y))) pairs)
      (sort > (iota 10)))
