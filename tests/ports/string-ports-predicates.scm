(define in (open-input-string ""))
(define out (open-output-string))
(list (input-port? in) (output-port? out) (input-port? out) (output-port? in)
      (char-ready? in) (eof-object? (read-char in)) (textual-port? out))
