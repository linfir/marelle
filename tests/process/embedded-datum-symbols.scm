;; The datum's symbols are the interned ones, before and after other symbols are made
(let* ((d (embedded-datum "d"))
       (fresh (string->symbol "fresh-symbol")))
  (list (eq? (car d) 'car)
        (eq? (cadr d) (string->symbol "lambda"))
        (eq? (caddr d) (string->symbol (string #\z #\z)))
        (eq? (vector-ref (cadddr d) 0) 'car)
        (eq? fresh 'fresh-symbol)
        (symbol->string (caddr d))))
