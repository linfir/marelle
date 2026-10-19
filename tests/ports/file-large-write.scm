;; A write larger than the buffer goes straight to the file
(call-with-output-file "big.txt"
  (lambda (p)
    (display "x" p)
    (write-string (list->string (make-list 20000 #\b)) p)
    (display "y" p)))
(call-with-input-file "big.txt"
  (lambda (p)
    (let ((s (read-string 30000 p)))
      (list (string-length s) (string-ref s 0) (string-ref s 1) (string-ref s 20001)))))
