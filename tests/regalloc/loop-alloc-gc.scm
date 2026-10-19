(let loop ((i 0) (l '()) (k 5))
  (if (= i 2000) (+ (length l) k) (loop (+ i 1) (cons i l) k)))
