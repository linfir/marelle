;; Each evaluation of the form makes a new type
(define (make-type)
  (define-record-type cell (fields (mutable value)))
  (cons make-cell cell?))
(define t1 (make-type))
(define t2 (make-type))
(define c ((car t1) 5))
(list ((cdr t1) c) ((cdr t2) c))
---
stdout = "(#t #f)"
