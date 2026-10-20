;; Symbols, case-sensitive, peculiar ones and escapes
(let ((p (open-input-string "abc ABC + - ... ->x a\\x41;b a.b")))
  (let next ((x (read p))
             (n 0))
    (if (eof-object? x)
        n
        (begin
          (write x)
          (newline)
          (next (read p) (+ n 1))))))
