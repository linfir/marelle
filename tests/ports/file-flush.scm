(define out (open-output-file "out.txt"))
(display "data" out)
(flush-output-port out)
(read-line (open-input-file "out.txt"))
