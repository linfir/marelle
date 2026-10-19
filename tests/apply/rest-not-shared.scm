;; The rest list is fresh, not the list given to apply
(define l (list 1 2))
(eq? l (apply (lambda r r) l))
