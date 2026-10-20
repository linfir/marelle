;; Line, nested block and datum comments
(let ((p (open-input-string "; line\n1 #| a #| b |# c |# 2 #;(3 4) 5 #;#;6 7 8 (9 #;10)")))
  (let next ((x (read p))
             (n 0))
    (if (eof-object? x)
        n
        (begin
          (write x)
          (newline)
          (next (read p) (+ n 1))))))
