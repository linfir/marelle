;; inc is inlined into f at both calls: its add-numbers slow path appears three times (f, called
;; twice, stays a function)
(define (inc x) (+ x 1))
(define (f a b) (* (inc a) (inc b)))
(list (f 3 4) (f 5 6))
