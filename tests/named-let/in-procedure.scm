(define (count-down n) (let loop ((n n) (steps 0)) (if (= n 0) steps (loop (- n 1) (+ steps 1)))))
(count-down 1000000)
