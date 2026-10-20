;; Lists, dotted, bracketed and nested
(let ((p (open-input-string "(1 2 3) (a . b) (a b . c) [1 2] (1 (2 (3)) ()) ()")))
  (let next ((x (read p))
             (n 0))
    (if (eof-object? x)
        n
        (begin
          (write x)
          (newline)
          (next (read p) (+ n 1))))))
