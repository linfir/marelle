(let ((a (embedded-file "a")))
  (bytevector-u8-set! a 0 66)
  (list a (embedded-file "a")))
