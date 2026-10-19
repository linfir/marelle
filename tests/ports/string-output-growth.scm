;; Past 10,000 chars, the buffer doubled from 64 bytes, then a UTF-32 char
(define p (open-output-string))
(do ((i 0 (+ i 1)))
    ((= i 3000))
  (display i p)
  (write-char #\space p))
(write-char #\é p)
(define s (get-output-string p))
(list (string-length s) (string-ref s 0) (string-ref s (- (string-length s) 1)) (substring s 0 10))
