;; string->number: #f on bad syntax, an optional radix
(list (map string->number (quote ("-17" "#x1F" "1/2" "#e1.5" "12345678901234567890" "1/0" "abc" "+" "" " 1" "1e")))
      (string->number "ff" 16)
      (string->number "101" 2)
      (string->number "#d10" 16)
      (string->number "z" 16)
      (string->number "z" 36))
