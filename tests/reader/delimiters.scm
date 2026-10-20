;; Quotes, # and ; end a token
(let ((p (open-input-string "a'b a,c a#t a;d\n e")))
  (let next ((x (read p))
             (n 0))
    (if (eof-object? x)
        n
        (begin
          (write x)
          (newline)
          (next (read p) (+ n 1))))))
