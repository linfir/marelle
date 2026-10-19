(define ps (vector (make-parameter 'a) (make-parameter 'b)))
(list (parameterize (((vector-ref ps 1) 'c)) ((vector-ref ps 1))) ((vector-ref ps 1)))
