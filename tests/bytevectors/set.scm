(let ((bv (make-bytevector 9 0)))
  (bytevector-u8-set! bv 8 200)
  (bytevector-u8-set! bv 0 1)
  bv)
