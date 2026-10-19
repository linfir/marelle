;; Vectors of pairs survive collections in a small heap
(define (build n)
  (let ((v (make-vector n 0)))
    (let loop ((i 0))
      (if (= i n)
          v
          (begin
            (vector-set! v i (cons i (make-vector 2 i)))
            (loop (+ i 1)))))))
(define (sum v)
  (let loop ((i 0) (acc 0))
    (if (= i (vector-length v))
        acc
        (loop (+ i 1) (+ (+ acc (car (vector-ref v i))) (vector-ref (cdr (vector-ref v i)) 1))))))
(define (repeat k acc)
  (if (= k 0) acc (repeat (- k 1) (sum (build 100)))))
(repeat 100 0)
---
stdout = "9900"
env = { SCHEME_GC_STRESS = "1000" }
