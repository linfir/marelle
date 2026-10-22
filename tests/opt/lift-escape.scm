;; A closure that is also a value is not lifted
(define (f k)
  (define (add x) (+ x k))
  (list (add 1) add))
(let ((r (f 10)))
  (list (car r) ((cadr r) 5)))
