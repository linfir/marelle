(define (add1 x)
  (declare unsafe)
  (+ x 1))

(define (twice f x)
  (declare unsafe)
  (f (f x)))

;; Indirect tail call in a loop
(define (repeat f n x)
  (declare unsafe)
  (if (= n 0)
      x
      (repeat f (- n 1) (f x))))

(define (count n)
  (declare unsafe)
  (if (= n 0) 0 ((car (cons count '())) (- n 1))))

(define (entry)
  (declare unsafe)
  (let ((g add1))
    (count 1000000)
    (cons (g 1)
          (cons (twice add1 5)
                (cons (repeat add1 1000 0) twice)))))
(entry)
---
env = { SCHEME_HEAP = "5000" }
stdout = "(2 7 1000 . #<procedure twice>)"
