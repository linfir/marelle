;; The unused car's load goes, its pair check stays
(define (f x) (let ((y (car x))) x))
(list (f '(1)) (f '(2 3)))
