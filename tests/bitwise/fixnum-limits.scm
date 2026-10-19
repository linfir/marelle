(define max 1152921504606846975)
(define min -1152921504606846976)
(list (bitwise-not max) (bitwise-not min) (bitwise-xor max min) (bitwise-and max min)
      (bitwise-ior max min))
