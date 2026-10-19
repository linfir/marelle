(call-with-output-file "out.txt"
  (lambda (p)
    (display "héllo" p)
    (newline p)
    (write '(1 "two") p)))
(call-with-input-file "out.txt"
  (lambda (p)
    (let* ((a (read-line p))
           (b (read-line p))
           (c (read-line p)))
      (list a b c))))
