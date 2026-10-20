;; Ratios, normalized
(let ((p (open-input-string "1/2 -1/2 6/4 4/2 #b-101/11")))
  (let next ((x (read p))
             (n 0))
    (if (eof-object? x)
        n
        (begin
          (write x)
          (newline)
          (next (read p) (+ n 1))))))
