(letrec* ((a (begin (set! b 1) 2))
          (b 3))
  a)
