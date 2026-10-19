(define greeting "héllo")
(define (get) greeting)
(define (inner) (let ((f (lambda () "inner"))) (f)))
(list (get) (inner) (string=? (get) "héllo"))
