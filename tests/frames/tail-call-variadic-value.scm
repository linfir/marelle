;; A variadic procedure value called with more arguments: a placeholder frame on the heap, then
;; apply-frame, in a loop of tail calls
(define (count-down self n . rest)
  (if (= n 0) (length rest) (self self (- n 1) 'x 'y)))
(define (run f)
  (f f 5000000))
(run count-down)
