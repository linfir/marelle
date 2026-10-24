(list (string<=? "abc" "abd") (string<=? "abc" "abc") (string<=? "abd" "abc") (string<=? "ab" "abc")
      (string>? "abd" "abc") (string>? "abc" "abc") (string>? "abc" "abd") (string>? "abc" "ab")
      (string>=? "abd" "abc") (string>=? "abc" "abc") (string>=? "abc" "abd") (string>=? "" "")
      (string>? "é" "z") (string<=? "" "a"))
