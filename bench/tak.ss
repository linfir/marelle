;; Calls with three arguments
(define (tak x y z)
  (if (not (< y x))
      z
      (tak (tak (- x 1) y z)
           (tak (- y 1) z x)
           (tak (- z 1) x y))))
(let loop ((i 0) (r 0))
  (if (= i 20)
      r
      (loop (+ i 1) (tak 22 16 8))))
