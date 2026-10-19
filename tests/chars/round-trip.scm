(define (check n) (if (= n 300) #t (if (= (char->integer (integer->char n)) n) (check (+ n 1)) n)))
(check 0)
