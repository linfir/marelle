;; inc is inlined into f at both calls: its add-numbers slow path appears three times
(define (inc x) (+ x 1))
(define (f a b) (* (inc a) (inc b)))
(f 3 4)
