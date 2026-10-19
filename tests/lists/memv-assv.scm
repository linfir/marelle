(define big (* 1000000000000 1000000000000))
(list (memv (* 1000000 1000000 1000000 1000000) (list 1 big 2)) (memv 3 '(1 2))
      (assv 2 '((1 . a) (2 . b))) (assv (+ big 0) (list (cons big 'big))) (assv 3 '()))
