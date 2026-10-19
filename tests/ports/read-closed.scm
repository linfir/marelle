(call-with-output-file "out.txt" (lambda (p) (display "x" p)))
(define p (open-input-file "out.txt"))
(close-input-port p)
(read-char p)
