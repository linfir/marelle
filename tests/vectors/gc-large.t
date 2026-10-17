;; A vector larger than the free space, after garbage
(define (garbage n) (if (= n 0) 0 (begin (cons n n) (garbage (- n 1)))))
(begin
  (garbage 1000)
  (vector-length (make-vector 3000 #t)))
---
stdout = "3000"
env = { SCHEME_HEAP = "4000" }
