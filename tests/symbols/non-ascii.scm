(list 'héllo
      (eq? 'héllo (string->symbol (string-append "hé" "llo")))
      (eq? 'λ (string->symbol (list->string (list (integer->char 955))))))
