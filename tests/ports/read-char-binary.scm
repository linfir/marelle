(call-with-output-file "out.txt" (lambda (p) (display "x" p)))
(read-char (open-file-input-port "out.txt"))
