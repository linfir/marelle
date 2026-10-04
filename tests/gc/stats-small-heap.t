(define (build n acc)
  (if (= n 0)
      acc
      (build (- n 1) (cons n acc))))
(define (sum l acc)
  (if (null? l)
      acc
      (sum (cdr l) (+ acc (car l)))))
(sum (build 1000 '()) 0)
---
env = { SCHEME_HEAP = "2500", SCHEME_GC_STATS = "1" }
stdout = "500500"
stderr_contains = " max live words\n"
chez = "skip"
