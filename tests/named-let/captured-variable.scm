(let loop ((i 0) (fs '()))
  (if (= i 3)
      (map (lambda (f) (f)) fs)
      (loop (+ i 1) (cons (lambda () i) fs))))
