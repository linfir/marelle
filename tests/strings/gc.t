;; Strings built in a loop survive collections, and so do the literals
(define (digit n) (string (integer->char (+ 48 (remainder n 10)))))
(define (build n acc)
  (if (= n 0)
      acc
      (build (- n 1) (cons (string-append (digit n) "é" (digit (quotient n 10))) acc))))
(define (total l acc)
  (if (null? l)
      acc
      (total (cdr l) (+ acc (char->integer (string-ref (car l) 0)) (string-length (car l))))))
(define l (build 2000 '()))
(list (total l 0) (car l) (car (last-pair l)) "lit")
---
stdout = '(111000 "1é0" "0é0" "lit")'
env = { SCHEME_HEAP = "30000" }
