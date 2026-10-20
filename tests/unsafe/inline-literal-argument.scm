;; A literal argument of an inline def is loaded at each use, the same object
(define (both x)
  (declare inline)
  (cons x x))
(let ((p (both '(1 2)))
      (q (both "ab")))
  (list (eq? (car p) (cdr p)) (eq? (car q) (cdr q)) p))
