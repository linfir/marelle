;; Arguments that call and allocate while the frame is half built, in a small heap
(define (build n acc) (if (= n 0) acc (build (- n 1) (cons n acc))))
(define (f a b c d e f g h) (+ (length a) (+ (length d) (length h))))
(define (repeat k acc)
  (if (= k 0)
      acc
      (repeat (- k 1) (f (build 50 (quote ())) 2 3 (build 60 (quote ())) 5 6 7 (build 70 (quote ()))))))
(repeat 200 0)
---
stdout = "180"
env = { SCHEME_HEAP = "2000" }
