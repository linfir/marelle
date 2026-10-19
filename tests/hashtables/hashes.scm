(list (= (equal-hash (list 1 "a" 'b)) (equal-hash (list 1 (string #\a) 'b)))
      (= (string-hash "hello") (string-hash (string #\h #\e #\l #\l #\o)))
      (= (symbol-hash 'x) (symbol-hash (string->symbol "x")))
      (integer? (equal-hash (vector 1 2))))
