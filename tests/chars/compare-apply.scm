(list (apply char<? (list #\a #\b #\c #\d)) (apply char=? (list #\a)) (map char<? (list #\a #\b) (list #\b #\a)))
