(define (pow b n) (if (= n 0) 1 (* b (pow b (- n 1)))))
(define a (pow 2 100))
(define b (+ (pow 2 100) 1))
(define c (pow 2 200))
(list-tail
 (cons 0 (cons (< a b) (cons (> a b) (cons (< a c) (cons (< c a) (cons (< (- 0 c) a)
         (cons (< (- 0 a) (- 0 b)) (cons (= a (- b 1)) (cons (= a b) '())))))))))
 1)
