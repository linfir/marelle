(letrec ((loop (lambda (n acc) (if (= n 0) acc (loop (- n 1) (+ acc 1)))))) (loop 1000000 0))
