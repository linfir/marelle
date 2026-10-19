(define acc '())
(vector-for-each (lambda (x) (set! acc (cons x acc))) (vector 1 2 3))
(vector-for-each (lambda (x y) (set! acc (cons (+ x y) acc))) (vector 1 2) (vector 10 20))
acc
