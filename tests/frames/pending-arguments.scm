;; Frames being filled across collections: the arguments stored so far are roots, the others
;; zeroed
(define (make n)
  (if (= n 0) '() (cons n (make (- n 1)))))
(define (sum l)
  (if (null? l) 0 (+ (car l) (sum (cdr l)))))
(define (sum3 a b c)
  (+ (sum a) (sum b) (sum c)))
(define (lst . xs) xs)
(list (sum3 (make 10) (make 20) (make 30))
      (map sum (lst (make 3) (make 4) (make 5))))
