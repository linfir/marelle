;; string-set! relocates the ASCII string to UTF-32: string-ref must not reuse the header test
(define (f s)
  (string-set! s 0 #\x3bb)
  (string-ref s 0))
(char->integer (f (string #\a #\b)))
