;; Integers of any size, signed
(let ((p (open-input-string "0 42 -42 +7 -0 123456789012345678901234567890")))
  (let next ((x (read p))
             (n 0))
    (if (eof-object? x)
        n
        (begin
          (write x)
          (newline)
          (next (read p) (+ n 1))))))
