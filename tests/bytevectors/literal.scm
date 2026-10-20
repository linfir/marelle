;; One shared object per literal, as in Chez
(define (f) (quote #vu8(1 2 3)))
(list (f) (eq? (f) (f)) (quasiquote (#vu8(4) (unquote (+ 1 2)))))
