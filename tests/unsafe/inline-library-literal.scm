;; read-datum, inlined, compares with the library's end marker: its literal must be the
;; library's object, not a copy of the program's
(define (count-data p)
  (declare unsafe)
  (let loop ((n 0))
    (if (eof-object? (read-datum p))
        n
        (loop (+ n 1)))))
(count-data (open-input-string "a (b c) 4"))
