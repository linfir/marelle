;; A static UTF-8 literal, indexed (its cache written) and compared
(let ((s "h\xe9;llo w\x3bb;rld"))
  (list (string-ref s 1) (string-ref s 7) (string-ref s 2) (string-length s)
        (string=? s (string-append "h\xe9;llo " "w\x3bb;rld"))))
