;; Quote abbreviations
(let ((p (open-input-string "'a `(a ,b ,@c) ''a")))
  (let next ((x (read p))
             (n 0))
    (if (eof-object? x)
        n
        (begin
          (write x)
          (newline)
          (next (read p) (+ n 1))))))
