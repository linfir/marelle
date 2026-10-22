;; Also used as a value: stays a function
(define (f l)
  (define (len l n) (if (null? l) n (len (cdr l) (+ n 1))))
  (list (len l 0) (map (lambda (x) (len x 0)) (list l l))))
(f '(1 2))
