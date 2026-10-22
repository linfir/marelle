;; A closure never used: its allocation and stores go
(define (f x)
  (let ((g (lambda () x)))
    (+ x 1)))
(f 1)
