;; The second read follows a write on one path: not shared; the tag test is
(define (f v b)
  (let ((a (vector-ref v 0)))
    (if b (vector-set! v 0 9))
    (+ a (vector-ref v 0))))
(list (f (vector 1) #t) (f (vector 1) #f))
