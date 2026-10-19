(with-output-to-string
  (lambda ()
    (write 1)
    (display (with-output-to-string
              (lambda ()
                (write "in"))))
    (newline)
    (write #\2)))
