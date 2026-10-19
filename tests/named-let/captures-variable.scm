(define (f n)
  (let loop ((i 0))
    (if (= i n) (list i) (cons i (loop (+ i 1))))))
(f 3)
