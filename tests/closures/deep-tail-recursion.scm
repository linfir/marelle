(define (make-loop step) (lambda (self n acc) (if (= n 0) acc (self self (- n 1) (+ acc step)))))
(let ((loop (make-loop 2))) (loop loop 1000000 0))
