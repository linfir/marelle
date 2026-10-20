(let ((bv (bytevector 10 20 255)))
  (+ (bytevector-u8-ref bv 0) (bytevector-u8-ref bv 2)))
