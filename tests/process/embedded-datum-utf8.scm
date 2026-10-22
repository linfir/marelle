;; Static UTF-8 strings, in .data: indexed forward, backward and jumping (their cache written),
;; compared with heap strings, and a static symbol of a non-ASCII name found by string->symbol
(define d (embedded-datum "d"))
(define s (car d))
(define (codes order) (map (lambda (i) (char->integer (string-ref s i))) order))
(list (codes (iota (string-length s)))
      (codes (reverse (iota (string-length s))))
      (codes '(5 0 6 1 4))
      (string=? s (string-append "aé€" "😀bλλ"))
      (equal? (cadr d) (list "λ" "x"))
      (eq? (caddr d) (string->symbol (string #\λ #\x)))
      (substring s 2 5)
      (caddr d))
