;; An unnamed gensym's name is made on demand, then kept
(define g (gensym))
(define junk (map (lambda (i) (make-vector 50 i)) (iota 300)))
(let ((s (symbol->string g)))
  (list g s (eq? s (symbol->string g)) (string->symbol s) (eq? g (string->symbol s))))
