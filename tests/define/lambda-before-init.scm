;; Procedures are bound first, unchecked, as in a body (Chez reports it)
(define a (f 1))
(define f (lambda (x) x))
a
