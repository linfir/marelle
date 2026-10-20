;; An invalid or truncated sequence, its lead byte and continuation bytes, is one U+FFFD, as in
;; Chez's utf8->string
(map char->integer
     (string->list (utf8->string (bytevector 255 65 195 226 130 237 160 128 192 128 245 128 240 159))))
