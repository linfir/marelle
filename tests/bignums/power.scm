(define (pow b n acc) (if (= n 0) acc (pow b (- n 1) (* acc b))))
(pow 2 200 1)
