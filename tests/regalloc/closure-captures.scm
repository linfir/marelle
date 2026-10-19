(define (g a b c d e f)
  (let ((h (lambda (x) (+ x (+ a (+ b (+ c (+ d (+ e f)))))))))
    (+ (h 1) (h 2))))
(g 1 2 3 4 5 6)
