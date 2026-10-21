(define p (open-file-output-port "out.bin"))
(define i (open-input-string "x"))
(list (binary-port? p) (textual-port? p) (output-port? p) (input-port? p) (port? p)
      (binary-port? i) (textual-port? i) (binary-port? (current-output-port)))
