(let ((p (open-input-string "#\\foo")))
  (let next ((x (read p))
             (n 0))
    (if (eof-object? x)
        n
        (begin
          (write x)
          (newline)
          (next (read p) (+ n 1))))))
