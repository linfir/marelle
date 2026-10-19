(define g (gensym "x"))
(list (symbol? g) (gensym? g) (gensym? 'x) (gensym? "x") (eq? g 'x) (eq? g (gensym "x"))
      (symbol->string g))
