;; A read before a loop is not shared with the loop body's, which follows a write
(define (f v)
  (let ((a (vector-ref v 0)))
    (let loop ((i 0))
      (when (< i 3)
        (vector-set! v 0 (+ (vector-ref v 0) 1))
        (loop (+ i 1))))
    (list a (vector-ref v 0))))
(f (vector 1))
