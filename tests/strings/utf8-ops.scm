;; UTF-8 strings built in different ways compare, hash and print alike; ASCII parts of them
;; are ASCII again
(define a (string-append "x" "é€" "😀y"))
(define b (list->string (list #\x #\é #\€ #\😀 #\y)))
(define c (utf8->string (string->utf8 a)))
(define d (substring (string-append "--" a "--") 2 7))
(list (string=? a b) (string=? a c) (string=? a d)
      (= (string-hash a) (string-hash b) (string-hash c) (string-hash d))
      (equal? (list a) (list d))
      (string<? "é" "€") (string<? "€" "é") (string<? "x€" "xé€")
      (substring a 3 5) (substring a 1 3) (string=? (substring a 4 5) "y")
      (eq? (string->symbol a) (string->symbol d))
      (string->utf8 (substring a 1 4))
      a)
