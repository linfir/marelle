;; A raw is not shared across an allocation
(define (f p)
  (let ((a (car p)))
    (let ((q (cons a a)))
      (list q (cdr p)))))
(f (cons 1 2))
