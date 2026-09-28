(define (make-adder n) (lambda (x) (+ x n)))

(define (adders n) (if (= n 0) '() (cons (make-adder n) (adders (- n 1)))))
(define (apply-all fs x) (if (null? fs) '() (cons ((car fs) x) (apply-all (cdr fs) x))))
(apply-all (adders 3) 10)
---
stdout = "(13 12 11)"
