(define p (open-file-output-port "out.bin"))
(close-port p)
(list p (open-file-input-port "out.bin"))
