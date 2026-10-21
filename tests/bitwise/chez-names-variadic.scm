(define (call f . args) (apply f args))
(list (logand) (logior) (logxor) (logand 7) (logor 3) (logand 15 6 12) (logior 1 2 4 8)
      (logxor 1 2 3) (call logand 6 3) (call logxor 5 1 4) (fold-left logior 0 (list 1 16 256))
      (logand 1180591620717411303424 2361183241434822606847 -1))
