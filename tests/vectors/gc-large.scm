;; A vector larger than the free space, after garbage (kept in sink until replaced, so not
;; optimized away)
(define sink #f)
(define (garbage n) (if (= n 0) 0 (begin (set! sink (cons n n)) (garbage (- n 1)))))
(begin
  (garbage 1000)
  (vector-length (make-vector 3000 #t)))
