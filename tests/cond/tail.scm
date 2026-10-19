(define (loop n) (cond ((= n 0) 7) ((< n 0) 0) (else (loop (- n 1)))))
(loop 1000000)
