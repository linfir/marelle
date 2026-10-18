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

(define (caar x)
  (declare inline public)
  (car (car x)))

(define (cadr x)
  (declare inline public)
  (car (cdr x)))

(define (cdar x)
  (declare inline public)
  (cdr (car x)))

(define (cddr x)
  (declare inline public)
  (cdr (cdr x)))

(define (caaar x)
  (declare inline public)
  (car (car (car x))))

(define (caadr x)
  (declare inline public)
  (car (car (cdr x))))

(define (cadar x)
  (declare inline public)
  (car (cdr (car x))))

(define (caddr x)
  (declare inline public)
  (car (cdr (cdr x))))

(define (cdaar x)
  (declare inline public)
  (cdr (car (car x))))

(define (cdadr x)
  (declare inline public)
  (cdr (car (cdr x))))

(define (cddar x)
  (declare inline public)
  (cdr (cdr (car x))))

(define (cdddr x)
  (declare inline public)
  (cdr (cdr (cdr x))))

(define (caaaar x)
  (declare inline public)
  (car (car (car (car x)))))

(define (caaadr x)
  (declare inline public)
  (car (car (car (cdr x)))))

(define (caadar x)
  (declare inline public)
  (car (car (cdr (car x)))))

(define (caaddr x)
  (declare inline public)
  (car (car (cdr (cdr x)))))

(define (cadaar x)
  (declare inline public)
  (car (cdr (car (car x)))))

(define (cadadr x)
  (declare inline public)
  (car (cdr (car (cdr x)))))

(define (caddar x)
  (declare inline public)
  (car (cdr (cdr (car x)))))

(define (cadddr x)
  (declare inline public)
  (car (cdr (cdr (cdr x)))))

(define (cdaaar x)
  (declare inline public)
  (cdr (car (car (car x)))))

(define (cdaadr x)
  (declare inline public)
  (cdr (car (car (cdr x)))))

(define (cdadar x)
  (declare inline public)
  (cdr (car (cdr (car x)))))

(define (cdaddr x)
  (declare inline public)
  (cdr (car (cdr (cdr x)))))

(define (cddaar x)
  (declare inline public)
  (cdr (cdr (car (car x)))))

(define (cddadr x)
  (declare inline public)
  (cdr (cdr (car (cdr x)))))

(define (cdddar x)
  (declare inline public)
  (cdr (cdr (cdr (car x)))))

(define (cddddr x)
  (declare inline public)
  (cdr (cdr (cdr (cdr x)))))

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

(define (list . xs)
  (declare public)
  xs)

(define (list-ref l k)
  (declare public)
  (car (list-tail l k)))

(define (list-copy l)
  (declare public)
  (if (pair? l)
      (cons (car l) (list-copy (cdr l)))
      l))

(define (last-pair l)
  (declare public)
  (if (pair? (cdr l))
      (last-pair (cdr l))
      l))

;; The fast walk takes two steps per step of the slow one, so it meets it on a cycle
(define (list? x)
  (declare public)
  (let loop ((slow x)
             (fast x))
    (cond
     ((null? fast) #t)
     ((not (pair? fast)) #f)
     ((null? (cdr fast)) #t)
     ((not (pair? (cdr fast))) #f)
     (else (let ((slow (cdr slow))
                 (fast (cddr fast)))
             (and (not (eq? slow fast))
                  (loop slow fast)))))))

(define (filter keep? l)
  (declare public)
  (reverse (fold-left (lambda (acc x) (if (keep? x) (cons x acc) acc))
                      '() l)))

;; SRFI 1: (f x acc), the first element as the initial acc
(define (reduce f default l)
  (declare public)
  (if (null? l)
      default
      (fold-left (lambda (acc x) (f x acc)) (car l) (cdr l))))

(define (fold-right f acc l)
  (declare public)
  (fold-left (lambda (acc x) (f x acc)) acc (reverse l)))
