;; 12001 bytes: past the 8 KiB buffer, a 2-byte char straddling each refill
(call-with-output-file "big.txt"
  (lambda (p)
    (write-char #\a p)
    (do ((i 0 (+ i 1)))
        ((= i 6000))
      (write-char #\é p))))
(call-with-input-file "big.txt"
  (lambda (p)
    (let loop ((n 0)
               (other 0))
      (let ((c (read-char p)))
        (cond
         ((eof-object? c) (list n other))
         ((or (char=? c #\é) (char=? c #\a)) (loop (+ n 1) other))
         (else (loop (+ n 1) (+ other 1))))))))
