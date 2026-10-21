(call-with-output-file "out.txt" (lambda (p) (display "x" p)))
(chmod "out.txt" (quote x))
