(list (number->string 0) (number->string 42) (number->string -255 16) (number->string 1/3 2)
      (number->string -22/7 8) (number->string 35 36) (number->string (expt 10 18))
      (number->string (expt 10 40)) (number->string (- (expt 2 100)) 16)
      (number->string (expt 7 99) 36) (apply number->string (list 10 2)))
