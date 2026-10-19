(define a (* 340282366920938463463374607431768211456 3))
(define b (- 0 (* 18446744073709551616 5)))
(list (bitwise-and a b) (bitwise-ior a b) (bitwise-xor a b) (bitwise-not a) (bitwise-not b)
      (bitwise-and a -1) (bitwise-and b 255) (bitwise-ior b 1) (bitwise-xor -1 18446744073709551615))
