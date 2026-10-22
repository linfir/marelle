;; Equal literals of a compilation are one static object; a procedure's literal is the same
;; object at each call
(define (f) '(1 2 (3)))
(define (g) '(1 2 (3)))
(list (eq? (f) (f)) (eq? (f) (g)) (eq? '#(a "b") '#(a "b")) (eq? "x" "x"))
