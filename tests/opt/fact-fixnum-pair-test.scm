;; Each branch of a pair test, then a fixnum test
(define (f x)
  (if (pair? x)
      (if (fixnum? x) 'both 'pair)
      (if (fixnum? x) 'fixnum 'neither)))
(list (f '(1)) (f 1) (f "s"))
