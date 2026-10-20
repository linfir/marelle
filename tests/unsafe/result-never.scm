;; A safe def that never returns, called in an arm
(define (fail x)
  (declare (result never))
  (error (quote fail) "bad" x))
(if (pair? (list 1)) 1 (fail 2))
