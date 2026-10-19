;; Two types with the same fields are distinct
(define-record-type a (fields x))
(define-record-type b (fields x))
(define r (make-a 1))
(list (a? r) (b? r) (a? (vector r)) (a? (cons r r)) (a? 5) (a? #f) (a? '()))
---
stdout = "(#t #f #f #f #f #f #f)"
