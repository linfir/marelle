(define (both f n m) (call-with-values (lambda () (f n m)) list))
(list (both floor/ 7 2) (both floor/ -7 2) (both floor/ 7 -2) (both floor/ -7 -2)
      (both truncate/ 7 2) (both truncate/ -7 2) (both truncate/ 7 -2) (both truncate/ -7 -2)
      (floor-quotient -100000000000000000000 3) (floor-remainder -100000000000000000000 3)
      (truncate-quotient -100000000000000000000 3) (truncate-remainder -100000000000000000000 3))
---
stdout = "((3 1) (-4 1) (-4 -1) (3 -1) (3 1) (-3 -1) (-3 1) (3 -1) -33333333333333333334 2 -33333333333333333333 -1)"
