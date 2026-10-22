;; An embedded datum's symbols are the literals', and the library's
(let ((d (car (embedded-datum "d"))))
  (list (eq? (car d) 'foo) (eq? (cadr d) 'pair) (eq? (car d) (string->symbol "foo"))))
