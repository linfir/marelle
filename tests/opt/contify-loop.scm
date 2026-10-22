;; An internal loop called once, not in tail position, becomes a loop of its caller
(define (f l)
  (define (sum l acc) (if (null? l) acc (sum (cdr l) (+ acc (car l)))))
  (* 2 (sum l 0)))
(f '(1 2 3))
