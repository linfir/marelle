(define (build n acc)
  (if (= n 0)
      acc
      (build (- n 1) (cons n acc))))
(define (sum l acc)
  (if (null? l)
      acc
      (sum (cdr l) (+ acc (car l)))))
(define (repeat k l acc)
  (if (= k 0)
      acc
      (repeat (- k 1) l (sum l 0))))
(repeat 200 (build 1000 '()) 0)
---
env = { SCHEME_GC_STRESS = "1000" }
stdout = "500500"
