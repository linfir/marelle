(with-output-to-file "out.txt"
  (lambda ()
    (display "abc")
    (write-char #\é)))
(display "after")
(with-input-from-file "out.txt"
  (lambda ()
    (read-string 10)))
