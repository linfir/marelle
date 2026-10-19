(let*-values (((a b) (values 1 2)) ((c) (values (+ a b))) ((a) a)) (list a b c))
