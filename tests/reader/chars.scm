;; Chars, names and hex
(let ((p (open-input-string "#\\a #\\A #\\( #\\) #\\x #\\x41 #\\space #\\nul #\\linefeed #\\rubout #\\tab #\\λ")))
  (let next ((x (read p))
             (n 0))
    (if (eof-object? x)
        n
        (begin
          (write x)
          (newline)
          (next (read p) (+ n 1))))))
