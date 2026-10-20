;; Strings with escapes and a line continuation
(let ((p (open-input-string "\"a\\nb\\t\\\"q\\\"\\\\\" \"\\x41;bc\" \"a\\\n   b\" \"λ\"")))
  (let next ((x (read p))
             (n 0))
    (if (eof-object? x)
        n
        (begin
          (write x)
          (newline)
          (next (read p) (+ n 1))))))
