;; Crible d'Érathostène
(define (eratosthenes n)
  (define vec (make-vector (if (< n 3) 0 (quotient (- n 2) 2)) #f))

  (define (mark! k)
    (vector-set! vec (quotient (- k 3) 2) #t))

  (define (marked? k)
    (vector-ref vec (quotient (- k 3) 2)))

  (define limit
    (if (< n 2)
        0
        (let-values (((s r) (exact-integer-sqrt (- n 1))))
          s)))

  (define (mark-all!)
    (let loop ((i 5) (a 2))
      (when (<= i limit)
        (unless (marked? i)
          (mark-multiples! i))
        (loop (+ i a) (- 6 a)))))

  (define (mark-multiples! k)
    (let loop ((i (* k k)))
      (when (< i n)
        (mark! i)
        (loop (+ i k k)))))

  (define (collect)
    (let loop ((i (if (odd? n) (- n 2) (- n 1))) (acc '()))
      (cond
       ((< i 3) (if (> n 2) (cons 2 acc) acc))
       ((marked? i) (loop (- i 2) acc))
       (else (loop (- i 2) (cons i acc))))))

  (mark-multiples! 3)
  (mark-all!)
  (collect))
