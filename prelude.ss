(define (zero? n)
  (= n 0))

;; The modulo has n's sign, the remainder m's
(define (remainder m n)
  (let ((r (modulo m n)))
    (if (or (zero? r) (eq? (< r 0) (< m 0)))
        r
        (- r n))))

(define (even? n)
  (zero? (modulo n 2)))

(define (odd? n)
  (not (even? n)))

;; By squaring; no rationals and no error procedure, so a negative exponent fails in car
(define (expt b n)
  (cond
   ((< n 0) (car n))
   ((zero? n) 1)
   ((even? n) (let ((h (expt b (quotient n 2))))
                (* h h)))
   (else (* b (expt b (- n 1))))))

(define (fibo n)
  (cond
   ((= n 0) 0)
   ((= n 1) 1)
   (else (+ (fibo (- n 1)) (fibo (- n 2))))))

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

;; Characters: ASCII only
(define (char-upper-case? c)
  (and (char<=? #\A c) (char<=? c #\Z)))

(define (char-lower-case? c)
  (and (char<=? #\a c) (char<=? c #\z)))

(define (char-alphabetic? c)
  (or (char-upper-case? c) (char-lower-case? c)))

(define (char-numeric? c)
  (and (char<=? #\0 c) (char<=? c #\9)))

(define (char-whitespace? c)
  (or (char=? c #\space)
      (and (char<=? #\tab c) (char<=? c #\return))))

(define (char-upcase c)
  (if (char-lower-case? c)
      (integer->char (- (char->integer c) 32))
      c))

(define (char-downcase c)
  (if (char-upper-case? c)
      (integer->char (+ (char->integer c) 32))
      c))
