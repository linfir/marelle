(list (string->list "") (string->list "aé") (list->string '()) (list->string (list #\a #\b))
      (string #\x #\y) (string))
---
stdout = '(() (#\a #\é) "" "ab" "xy" "")'
