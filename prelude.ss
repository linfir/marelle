(define (zero? n)
  (= n 0))

(define (fibo n)
  (if (= n 0)
      0
      (if (= n 1)
          1
          (+ (fibo (- n 1)) (fibo (- n 2))))))

(define (fold-left f acc l)
  (if (null? l)
      acc
      (fold-left f (f acc (car l)) (cdr l))))

(define (length l)
  (fold-left (lambda (n x) (+ n 1)) 0 l))

(define (append a b)
  (if (null? a)
      b
      (cons (car a)
            (append (cdr a) b))))

(define (append-reverse a b)
  (if (null? a)
      b
      (append-reverse (cdr a) (cons (car a) b))))

(define (reverse l)
  (append-reverse l '()))

(define (list-tail l k)
  (if (= k 0)
      l
      (list-tail (cdr l) (- k 1))))

(define (map f l)
  (reverse (fold-left (lambda (acc x) (cons (f x) acc))
                      '() l)))

(define (for-each f l)
  (if (pair? l)
      (begin
        (f (car l))
        (for-each f (cdr l)))))
