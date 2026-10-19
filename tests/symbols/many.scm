;; Many fresh symbols: the table grows, and its symbols survive collections
(define (digits n)
  (if (< n 10)
      (string (integer->char (+ 48 n)))
      (string-append (digits (quotient n 10)) (string (integer->char (+ 48 (remainder n 10)))))))
(define (name n) (string->symbol (string-append "s" (digits n))))
(define (make n acc) (if (= n 0) acc (make (- n 1) (cons (name n) acc))))
(define syms (make 3000 '()))
(define (all-eq? l n)
  (cond
   ((null? l) #t)
   ((eq? (car l) (name n)) (all-eq? (cdr l) (+ n 1)))
   (else #f)))
(list (all-eq? syms 1) (car syms) (eq? (car syms) 's1) (eq? 's3000 (car (last-pair syms))))
