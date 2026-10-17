(define (pair? x)
  (declare unsafe inline public)
  (tag=? x (raw 3)))

(define (car x)
  (declare unsafe inline public)
  (unless (pair? x)
    (scheme-error "`car` expected a pair" x))
  (as-val (load (untag x) (raw 0))))

(define (cdr x)
  (declare unsafe inline public)
  (unless (pair? x)
    (scheme-error "`cdr` expected a pair" x))
  (as-val (load (untag x) (raw 1))))

(define (cons a d)
  (declare unsafe inline public)
  (let ((p (alloc (raw 2))))
    (store p (raw 0) a)
    (store p (raw 1) d)
    (as-val (bit-or p (raw 3)))))

(define (fold-left f acc l)
  (declare public)
  (if (null? l)
      acc
      (fold-left f (f acc (car l)) (cdr l))))

(define (length l)
  (declare public)
  (fold-left (lambda (n x) (+ n 1)) 0 l))

(define (append a b)
  (declare public)
  (if (null? a)
      b
      (cons (car a)
            (append (cdr a) b))))

(define (append-reverse a b)
  (declare public)
  (if (null? a)
      b
      (append-reverse (cdr a) (cons (car a) b))))

(define (reverse l)
  (declare public)
  (append-reverse l '()))

(define (list-tail l k)
  (declare public)
  (if (= k 0)
      l
      (list-tail (cdr l) (- k 1))))

(define (map f l)
  (declare public)
  (reverse (fold-left (lambda (acc x) (cons (f x) acc))
                      '() l)))

(define (for-each f l)
  (declare public)
  (if (pair? l)
      (begin
        (f (car l))
        (for-each f (cdr l)))))
