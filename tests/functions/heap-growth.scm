;; A live list outgrowing several sizes of the heap
(define (build n acc)
  (if (= n 0) acc (build (- n 1) (cons n acc))))
(define l (build 2000000 '()))
(length l)
