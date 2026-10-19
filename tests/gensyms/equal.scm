(define g (gensym "a"))
(list (equal? (list g) (list g)) (equal? (gensym "a") (gensym "a")) (eq? (string->symbol "a") 'a))
