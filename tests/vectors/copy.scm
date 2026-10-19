(define v (vector 1 2 3))
(define c (vector-copy v))
(vector-set! c 0 'x)
(list v c (vector-copy (vector)))
