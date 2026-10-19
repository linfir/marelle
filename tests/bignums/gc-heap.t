;; Bignums survive collections in a small heap
(define (pow b n) (if (= n 0) 1 (* b (pow b (- n 1)))))
(define (build n acc) (if (= n 0) acc (build (- n 1) (cons (+ (pow 2 100) n) acc))))
(define (sum l acc) (if (null? l) acc (sum (cdr l) (+ acc (car l)))))
(sum (build 2000 '()) 0)
---
stdout = "2535301200456458802993406412753000"
env = { SCHEME_GC_STRESS = "1000" }
