;; Branches over more than 127 bytes of code: rel32 jumps
(define (f x)
  (if (< x 0)
      (+ x (+ x (+ x (+ x (+ x (+ x (+ x (+ x (+ x (+ x (+ x x)))))))))))
      (- x (- x (- x (- x (- x (- x (- x (- x (- x (- x (- x x)))))))))))))
(+ (f 3) (f -1))
