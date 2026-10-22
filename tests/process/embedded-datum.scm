;; The forms of the file, as read
(let ((d (embedded-datum "d")))
  (list d
        (equal? d '((define (f x) (car x)) "h\x00e9;llo" #(a 1 #\b "s") 123456789012345678901234567890 1/3 (a . b) () #t #\x3bb))
        (embedded-datum "none")))
