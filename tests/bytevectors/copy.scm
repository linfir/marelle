;; The copies are fresh
(let* ((bv (bytevector 1 2 3 4))
       (c (bytevector-copy bv)))
  (bytevector-u8-set! c 0 9)
  (list bv c (bytevector-copy bv 1) (bytevector-copy bv 1 3) (bytevector-copy bv 4)))
