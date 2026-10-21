(call-with-output-file "out.txt" (lambda (p) (display "x" p)))
(define i (open-file-input-port "out.txt"))
(close-port i)
(get-u8 i)
