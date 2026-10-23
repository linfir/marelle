;; write shows a gensym's number when print-gensym is #t, #: when it is pretty; display never
(define a (gensym "a"))
(define b (gensym))
(define (shown write)
  (with-output-to-string (lambda () (write (list a b 'c)))))
(list (shown write)
      (parameterize ((print-gensym #t)) (list (print-gensym) (shown write) (shown display)))
      (parameterize ((print-gensym 'pretty)) (list (print-gensym) (shown write)))
      (print-gensym))
