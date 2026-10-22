;; A call whose continuation only returns its value is a tail call
(define (g x) x)
(define (f x) (let ((y (g x))) y))
(f 6)
