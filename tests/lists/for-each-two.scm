(define acc '())
(for-each (lambda (a b) (set! acc (cons (* a b) acc))) '(1 2 3) '(4 5 6))
(for-each (lambda (a b c) (set! acc (cons (+ a b c) acc))) '(1 2) '(3 4) '(5 6))
acc
