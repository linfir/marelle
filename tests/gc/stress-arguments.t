(define (pick a b c d e f)
  (cons (car a) (cons (car b) (cons (car c) (cons (car d) (cons (car e) (cons (car f) '())))))))
(define (pass a b c d e f)
  (pick a b c d e f))
(pass (cons 1 '()) (cons 2 '()) (cons 3 '()) (cons 4 '()) (cons 5 '()) (cons 6 '()))
---
env = { SCHEME_GC_STRESS = "1" }
stdout = "(1 2 3 4 5 6)"
