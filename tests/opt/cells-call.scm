;; A local assigned from calls, read after them
(define (g x) (+ x 1))
(define (f)
  (let ((n 0))
    (set! n (g n))
    (set! n (+ n (g n)))
    (list n (g n))))
(f)
