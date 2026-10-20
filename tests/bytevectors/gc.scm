;; Bytevectors through collections in a small heap
(define (fill n acc)
  (if (= n 0)
      acc
      (fill (- n 1) (cons (make-bytevector 13 (modulo n 256)) acc))))
(let ((l (fill 20000 (quote ()))))
  (let sum ((l l) (acc 0))
    (if (null? l)
        acc
        (sum (cdr l) (+ acc (bytevector-u8-ref (car l) 12))))))
