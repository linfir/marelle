;; Records holding records and closures
(define-record-type node (fields value next))
(define-record-type box (fields (mutable f)))
(define b (make-box (lambda (x) (* x 3))))
(define l (make-node 1 (make-node 2 #f)))
(list ((box-f b) (node-value (node-next l))) (node-next (node-next l)))
---
stdout = "(6 #f)"
