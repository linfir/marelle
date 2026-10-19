(define (counter-from n) (letrec ((g (lambda (k) (if (= k 0) n (g (- k 1)))))) g))
((counter-from 7) 3)
