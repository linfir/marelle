;; The first clause that takes the count wins
(define f (case-lambda ((x . r) (quote rest)) ((x) (quote one))))
(f 1)
