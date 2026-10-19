(define (shuffled n acc)
  (if (= n 0)
      acc
      (shuffled (- n 1) (cons (modulo (* n 7919) 10007) acc))))
(define s (sort < (shuffled 10000 '())))
(define (ordered? l)
  (or (null? (cdr l))
      (and (<= (car l) (cadr l)) (ordered? (cdr l)))))
(list (length s) (ordered? s))
