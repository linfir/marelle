(define f (case-lambda ((x) (define y (* x 2)) y) ((x y) (set! x 5) (+ x y))))
(list (f 1) (f 1 2))
