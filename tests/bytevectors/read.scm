(list (read (open-input-string "#vu8(1 255)"))
      (read (open-input-string "#u8( #x10 0 )"))
      (read (open-input-string "(#vu8() . #u8(2))")))
