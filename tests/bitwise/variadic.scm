(define (call f . args) (apply f args))
(list (bitwise-and) (bitwise-ior) (bitwise-xor) (bitwise-and 7) (bitwise-ior 1 2 4 8)
      (bitwise-xor 1 3 7 15) (bitwise-and 15 14 12) (call bitwise-ior 1 2 4) (call bitwise-not 0)
      (fold-left bitwise-xor 0 (list 1 2 3)))
