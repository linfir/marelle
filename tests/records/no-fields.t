(define-record-type unbound-value)
(define u (make-unbound-value))
(list (unbound-value? u) (unbound-value? 'u))
---
stdout = "(#t #f)"
