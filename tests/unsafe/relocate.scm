;; A relocated object forwards to its new body (resolve, put-datum) until a collection
;; redirects every pointer to it: here a vector, from a global, a list and a closure
(define (moved-to! old new)
  (declare unsafe)
  (relocate! old new)
  undef)
(define (body x)
  (declare unsafe)
  (resolve x))
(define (moved-object? x)
  (declare unsafe)
  (moved? x))

(define a (vector 1 2))
(define b (vector 1 2 3))
(define c (vector 1 2 3 4))
(define held (list a))
(define get (lambda () a))
(moved-to! a b)
(moved-to! b c)
(define before (list (moved-object? a) (moved-object? b) (eq? (body a) c) held))
(make-vector 2000000 0) ; a collection
(list before (moved-object? a) (eq? a c) (eq? (car held) c) (eq? (get) c) held)
