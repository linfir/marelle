(define (build n acc)
  (if (= n 0)
      acc
      (build (- n 1) (cons (cons n n) acc))))
(define (sum l)
  (if (null? l)
      0
      (+ (car (car l)) (sum (cdr l)))))
(let ((l (build 300 '())))
  (+ (sum (reverse l)) (length (append l l))))
