(define (root n) (call-with-values (lambda () (exact-integer-sqrt n)) list))
(list (root 0) (root 1) (root 2) (root 4) (root 17) (root 1152921504606846975)
      (root 100000000000000000000) (root (- (* 18446744073709551616 18446744073709551616) 1)))
