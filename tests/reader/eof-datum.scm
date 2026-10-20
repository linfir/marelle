;; #!eof reads as the EOF object, here in a list
(let ((p (open-input-string "(a #!eof) #!no-fold-case b")))
  (let next ((x (read p))
             (n 0))
    (if (eof-object? x)
        n
        (begin
          (write x)
          (newline)
          (next (read p) (+ n 1))))))
