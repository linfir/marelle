;; set-cdr! truncates, extends and makes a cycle, which list? sees
(define p (list 1 2 3))
(set-cdr! (cdr p) '())
(define q (cons 0 p))
(set-cdr! (cdr q) (list 7 8))
(define r (list 1 2))
(set-cdr! (cdr r) r)
(list p q (list? q) (list? r) (eq? (cddr r) r))
