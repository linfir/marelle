;; Unsafe defines see the library internals and the low operations
(define (tag-of x)
  (declare unsafe)
  (if (fixnum? x) (make-fixnum (bit-and (as-raw x) (raw 7))) (quote ())))
(cons (tag-of 5) (tag-of (cons 1 2)))
---
stdout = "(1)"
