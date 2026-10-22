;; Literal vectors and bytevectors, immutable heap objects, survive collections and are still
;; copied by vector-copy into mutable ones
(define (f) '#(1 (2 "three") #(4)))
(define (g) #vu8(5 6))
(let loop ((i 0) (keep '()))
  (if (< i 20000)
      (loop (+ i 1) (cons (make-vector 3 i) (if (pair? keep) (cdr keep) keep)))
      (let ((c (vector-copy (f))))
        (vector-set! c 0 9)
        (list (f) (g) c (eq? (f) (f)) (equal? (f) (vector 1 (list 2 "three") (vector 4)))))))
