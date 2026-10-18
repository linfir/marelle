(define e (list (cons (list 1) 'one) (cons "two" 2) (cons 3 'three)))
(list (assoc (list 1) e) (assoc "two" e) (assoc 4 e) (assoc 3 '()))
---
stdout = '(((1) . one) ("two" . 2) #f #f)'
