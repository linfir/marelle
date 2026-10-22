;; A pair is no fixnum: + goes straight to its general case
(define (f x) (if (pair? x) (+ x 1) x))
(f 5)
