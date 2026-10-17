(define (f a b c d e f g h)
  (declare public)
  (cons a h))
(f 1 2 3 4 5 6 7 8)
---
stdout = "(1 . 8)"
chez = "skip"
