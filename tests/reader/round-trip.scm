;; What write writes, read reads back
(let* ((d (quote (a (b . "c\\n") #(1 #\x #\space 2/3) -12345678901234567890 #t #f ())))
       (p (open-output-string)))
  (write d p)
  (equal? d (read (open-input-string (get-output-string p)))))
