;; The set! on the back edge kills the copy at the top of the loop
(let ((x 1))
  (let ((y x))
    (let loop ((i 0))
      (when (< i 3)
        (set! x (+ x 1))
        (loop (+ i 1))))
    (list x y)))
