;; A lifted loop whose captures (a list, a vector) move across collections
(define (f xs v)
  (define (loop i acc)
    (if (< i (vector-length v))
        (loop (+ i 1) (cons (+ (vector-ref v i) (length xs)) acc))
        acc))
  (loop 0 '()))
(f (list 1 2 3) (make-vector 2000 1))
(length (f (list 1 2 3) (make-vector 2000 1)))
