(define (make n) (values (make-vector n 1) (list n n)))
(define (loop i acc)
  (if (= i 0)
      acc
      (call-with-values (lambda () (make 10))
        (lambda (v l) (loop (- i 1) (+ acc (vector-ref v 0) (car l)))))))
(loop 1000 0)
---
env = { SCHEME_HEAP = "5000" }
stdout = "11000"
chez = "skip"
