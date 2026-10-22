;; Mutually recursive internal defines, entered once: both contified, no call left
(define (f n)
  (define (ev? n) (if (= n 0) #t (od? (- n 1))))
  (define (od? n) (if (= n 0) #f (ev? (- n 1))))
  (ev? n))
(list (f 10) (f 7))
