(define l (list 1 2 3 4 5))
(define t (list (list (list (list 1 2) 3) 4) 5))
(list (cadddr l) (cddddr l) (caaaar t) (cdaaar t) (cadaar t) (caadar (list (list 0 (list 7)))) (cdaddr (list 0 0 (list 8 9))) (caaddr (list 0 0 (list 6))))
---
stdout = "(4 (5) 1 (2) 3 7 (9) 6)"
