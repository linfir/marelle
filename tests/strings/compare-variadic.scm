(let ((l (list "a" "b" "c")))
  (list (string=? "x") (string=? "x" "x" "x") (string=? "x" "x" "y") (string<? "a" "b" "c")
        (string<? "a" "c" "b") (apply string<? l) (apply string=? "a" l)))
