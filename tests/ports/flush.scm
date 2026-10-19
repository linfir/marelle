(display "a")
(flush-output-port)
(display "b" (current-error-port))
(flush-output-port (current-error-port))
0
