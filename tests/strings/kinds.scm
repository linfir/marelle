;; Equal strings have the same kind, however they are built
(list (string=? (substring "é-abc" 2 5) "abc")
      (string=? (substring "aéb" 0 1) "a")
      (string=? (string-append "ab" "c") "abc")
      (string=? (string-append "a" "é") "aé")
      (string=? (list->string (list #\a #\é)) "aé")
      (string=? (string #\a #\b) "ab"))
