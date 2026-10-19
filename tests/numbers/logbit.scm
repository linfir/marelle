(define big (expt 2 70))
(list (logbit? 0 5) (logbit? 1 5) (logbit? 2 5) (logbit? 0 0) (logbit? 100 -1) (logbit? 60 -1)
      (logbit? 70 big) (logbit? 69 big) (logbit? 0 (- 0 big)) (logbit? 70 (- 0 big))
      (logbit? 200 (- 0 big)) (logbit? 60 (- (expt 2 60))))
