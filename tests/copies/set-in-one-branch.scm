;; The copy y of x survives only the branch without the set!
(define (f x c)
  (let ((y x))
    (if c (set! x 10))
    (+ x y)))
(list (f 1 #t) (f 1 #f))
