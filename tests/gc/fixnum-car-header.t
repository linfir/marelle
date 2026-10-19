;; Pairs whose car is a fixnum 48 to 51 survive collections: the GC must not take it for a
;; string header
(define (build n acc) (if (= n 0) acc (build (- n 1) (cons (+ 48 (remainder n 4)) acc))))
(define (sum l acc) (if (null? l) acc (sum (cdr l) (+ acc (car l)))))
(define (repeat k acc) (if (= k 0) acc (repeat (- k 1) (sum (build 500 '()) 0))))
(repeat 50 0)
---
stdout = "24750"
env = { SCHEME_GC_STRESS = "1000" }
