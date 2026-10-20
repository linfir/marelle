;; #e makes a decimal exact
(let ((p (open-input-string "#e1.5 #e1e3 #e-1.25e2 #e.5 #e1.5e-1")))
  (let next ((x (read p))
             (n 0))
    (if (eof-object? x)
        n
        (begin
          (write x)
          (newline)
          (next (read p) (+ n 1))))))
