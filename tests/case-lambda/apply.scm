(define f (case-lambda ((a b) (+ a b)) ((a) (- a))))
(list (apply f (list 1 2)) (apply f 5 (list)))
