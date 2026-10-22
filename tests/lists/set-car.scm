;; set-car! is seen through every reference to the pair
(define p (list 1 2 3))
(define l (list p p))
(define (first) (car p))
(set-car! p 'a)
(set-car! (cddr p) (vector 4))
(list p l (first) (eq? (car l) (cadr l)))
