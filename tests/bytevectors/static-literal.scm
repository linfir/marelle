;; A static bytevector literal, read and copied
(let ((b #vu8(1 2 3 250 0 7 8 9 10)))
  (list (bytevector-u8-ref b 3) (bytevector-u8-ref b 8) (bytevector-length b)
        (bytevector-copy b 6)))
