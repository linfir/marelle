(call-with-output-file "out.txt" (lambda (p) (display "x" p)))
(chmod "out.txt" 0)
(open-input-file "out.txt")
