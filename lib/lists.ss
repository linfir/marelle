;; A mutable pair is tagged 3, an immutable one (a literal or static) 7
(define (pair? x)
  (declare unsafe inline public)
  (raw= (bit-and (as-raw x) (raw 3)) (raw 3)))

;; Unchecked, for noalloc defs, which cannot call error
(define (pair-car x)
  (declare unsafe inline)
  (as-val (load (untag x) (raw 0))))

(define (pair-cdr x)
  (declare unsafe inline)
  (as-val (load (untag x) (raw 1))))

(define (car x)
  (declare unsafe inline public)
  (unless (pair? x)
    (argument-error car 'pair x))
  (pair-car x))

(define (cdr x)
  (declare unsafe inline public)
  (unless (pair? x)
    (argument-error cdr 'pair x))
  (pair-cdr x))

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

(define (check-mutable-pair p who)
  (declare unsafe inline (result void))
  (unless (tag=? p (raw 3))
    (argument-error who (if (pair? p) 'mutable-pair 'pair) p)))

(define (set-car! p x)
  (declare unsafe inline public)
  (check-mutable-pair p set-car!)
  (store (untag p) (raw 0) x)
  undef)

(define (set-cdr! p x)
  (declare unsafe inline public)
  (check-mutable-pair p set-cdr!)
  (set-tail! p x)
  undef)

(define (fold-left f acc l)
  (declare public)
  (if (null? l)
      acc
      (fold-left f (f acc (car l)) (cdr l))))

;; The cdr of a pair just allocated by the caller
(define (set-tail! p x)
  (declare unsafe inline (result void))
  (store (untag p) (raw 1) x))

(define (length l)
  (declare public)
  (let loop ((n 0) (l l))
    (if (null? l)
        n
        (loop (+ n 1) (cdr l)))))

;; Each new pair's cdr is b until the next one replaces it
(define (append a b)
  (declare public)
  (if (null? a)
      b
      (let ((head (cons (car a) b)))
        (let loop ((tail head) (a (cdr a)))
          (if (null? a)
              head
              (let ((p (cons (car a) b)))
                (set-tail! tail p)
                (loop p (cdr a))))))))

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
  (if (null? l)
      '()
      (let ((head (cons (f (car l)) '())))
        (let loop ((tail head) (l (cdr l)))
          (if (null? l)
              head
              (let ((p (cons (f (car l)) '())))
                (set-tail! tail p)
                (loop p (cdr l))))))))

(define (for-each f l)
  (declare public)
  (if (pair? l)
      (begin
        (f (car l))
        (for-each f (cdr l)))))

(define (list x)
  (declare inline public)
  (cons x '()))

(define (variadic-list . xs)
  xs)

(define (list-ref l k)
  (declare public)
  (car (list-tail l k)))

(define (list-copy l)
  (declare public)
  (if (pair? l)
      (let ((head (cons (car l) (cdr l))))
        (let loop ((tail head) (l (cdr l)))
          (if (pair? l)
              (let ((p (cons (car l) (cdr l))))
                (set-tail! tail p)
                (loop p (cdr l)))
              head)))
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
  (let ((head (cons #f '())))
    (let loop ((tail head) (l l))
      (if (null? l)
          (cdr head)
          (let ((x (car l)))
            (if (keep? x)
                (let ((p (cons x '())))
                  (set-tail! tail p)
                  (loop p (cdr l)))
                (loop tail (cdr l))))))))

;; SRFI 1: (f x acc), the first element as the initial acc
(define (reduce f default l)
  (declare public)
  (if (null? l)
      default
      (let loop ((acc (car l)) (l (cdr l)))
        (if (null? l)
            acc
            (loop (f (car l) acc) (cdr l))))))

(define (fold-right f acc l)
  (declare public)
  (let loop ((acc acc) (l (reverse l)))
    (if (null? l)
        acc
        (loop (f (car l) acc) (cdr l)))))

(define (memq x l)
  (declare public)
  (cond
   ((null? l) #f)
   ((eq? x (car l)) l)
   (else (memq x (cdr l)))))

(define (assq x l)
  (declare public)
  (cond
   ((null? l) #f)
   ((eq? x (caar l)) (car l))
   (else (assq x (cdr l)))))

(define (member x l)
  (declare public)
  (cond
   ((null? l) #f)
   ((equal? x (car l)) l)
   (else (member x (cdr l)))))

(define (member-with x l compare)
  (cond
   ((null? l) #f)
   ((compare x (car l)) l)
   (else (member-with x (cdr l) compare))))

(define (variadic-member x l . compare)
  (cond
   ((null? compare) (member x l))
   ((null? (cdr compare)) (member-with x l (car compare)))
   (else (argument-count-error member (+ 2 (length compare))))))

(define (assoc x l)
  (declare public)
  (cond
   ((null? l) #f)
   ((equal? x (caar l)) (car l))
   (else (assoc x (cdr l)))))

(define (assoc-with x l compare)
  (cond
   ((null? l) #f)
   ((compare x (caar l)) (car l))
   (else (assoc-with x (cdr l) compare))))

(define (variadic-assoc x l . compare)
  (cond
   ((null? compare) (assoc x l))
   ((null? (cdr compare)) (assoc-with x l (car compare)))
   (else (argument-count-error assoc (+ 2 (length compare))))))

(define (equal? x y)
  (declare public)
  (cond
   ((eqv? x y) #t)
   ((pair? x) (and (pair? y) (equal? (car x) (car y)) (equal? (cdr x) (cdr y))))
   ((string? x) (and (string? y) (string=? x y)))
   ((bytevector? x) (and (bytevector? y) (bytevector=? x y)))
   ((vector? x)
    (and (vector? y)
         (= (vector-length x) (vector-length y))
         (let loop ((i 0))
           (or (= i (vector-length x))
               (and (equal? (vector-ref x i) (vector-ref y i))
                    (loop (+ i 1)))))))
   (else #f)))

(define (memv x l)
  (declare public)
  (cond
   ((null? l) #f)
   ((eqv? x (car l)) l)
   (else (memv x (cdr l)))))

(define (assv x l)
  (declare public)
  (cond
   ((null? l) #f)
   ((eqv? x (caar l)) (car l))
   (else (assv x (cdr l)))))

(define (find ok? l)
  (declare public)
  (cond
   ((null? l) #f)
   ((ok? (car l)) (car l))
   (else (find ok? (cdr l)))))

(define (remq x l)
  (declare public)
  (filter (lambda (y) (not (eq? x y))) l))

;; The last call is a tail call, its value the result
(define (for-all ok? l)
  (declare public)
  (or (null? l)
      (let loop ((l l))
        (if (null? (cdr l))
            (ok? (car l))
            (and (ok? (car l)) (loop (cdr l)))))))

(define (exists ok? l)
  (declare public)
  (and (pair? l)
       (let loop ((l l))
         (if (null? (cdr l))
             (ok? (car l))
             (or (ok? (car l)) (loop (cdr l)))))))

;; Several lists: the cars, or #f when one ends, so the shortest one stops
(define (cars ls)
  (cond
   ((null? ls) '())
   ((pair? (car ls))
    (let ((rest (cars (cdr ls))))
      (and rest (cons (caar ls) rest))))
   (else #f)))

(define (cdrs ls)
  (map cdr ls))

(define (map2 f a b)
  (declare unsafe)
  (if (and (pair? a) (pair? b))
      (let ((head (cons (f (car a) (car b)) '())))
        (let loop ((tail head) (a (cdr a)) (b (cdr b)))
          (if (and (pair? a) (pair? b))
              (let ((p (cons (f (car a) (car b)) '())))
                (set-tail! tail p)
                (loop p (cdr a) (cdr b)))
              head)))
      '()))

(define (map-lists f ls)
  (declare unsafe)
  (let ((head (cons #f '())))
    (let loop ((tail head) (ls ls))
      (let ((xs (cars ls)))
        (if xs
            (let ((p (cons (apply f xs) '())))
              (set-tail! tail p)
              (loop p (cdrs ls)))
            (cdr head))))))

(define (variadic-map f l . ls)
  (cond
   ((null? ls) (map f l))
   ((null? (cdr ls)) (map2 f l (car ls)))
   (else (map-lists f (cons l ls)))))

(define (variadic-for-each f l . ls)
  (cond
   ((null? ls) (for-each f l))
   ((null? (cdr ls))
    (let loop ((a l) (b (car ls)))
      (when (and (pair? a) (pair? b))
        (f (car a) (car b))
        (loop (cdr a) (cdr b)))))
   (else
    (let loop ((ls (cons l ls)))
      (let ((xs (cars ls)))
        (when xs
          (apply f xs)
          (loop (cdrs ls))))))))

(define (variadic-for-all ok? l . ls)
  (let loop ((ls (cons l ls)) (last #t))
    (let ((xs (cars ls)))
      (if xs
          (let ((r (apply ok? xs)))
            (and r (loop (cdrs ls) r)))
          last))))

(define (variadic-exists ok? l . ls)
  (let loop ((ls (cons l ls)))
    (let ((xs (cars ls)))
      (and xs
           (or (apply ok? xs) (loop (cdrs ls)))))))

;; From the last list, which is shared, so each other is copied once
(define (variadic-append . ls)
  (if (null? ls)
      '()
      (let ((rev (reverse ls)))
        (fold-left (lambda (acc l) (append l acc)) (car rev) (cdr rev)))))

(define (cons* x . rest)
  (declare unsafe public)
  (if (null? rest)
      x
      (let ((head (cons x '())))
        (let loop ((tail head) (xs rest))
          (if (null? (cdr xs))
              (begin
                (set-tail! tail (car xs))
                head)
              (let ((p (cons (car xs) '())))
                (set-tail! tail p)
                (loop p (cdr xs))))))))

(define (check-count k who kind)
  (declare unsafe inline (result void))
  (unless (and (fixnum? k) (raw>= (fixnum-value k) (raw 0)))
    (argument-error who kind k)))

(define (iota n)
  (declare public)
  (check-count n iota 'nonnegative-fixnum)
  (let loop ((i n) (acc '()))
    (if (eq? i 0)
        acc
        (loop (- i 1) (cons (- i 1) acc)))))

(define (make-list k fill)
  (declare public)
  (check-count k make-list 'size)
  (let loop ((k k) (acc '()))
    (if (eq? k 0)
        acc
        (loop (- k 1) (cons fill acc)))))

(define (variadic-make-list k . fill)
  (declare unsafe)
  (cond
   ((null? fill) (make-list k undef))
   ((null? (cdr fill)) (make-list k (car fill)))
   (else (argument-count-error make-list (+ 1 (length fill))))))

(define (list-head l k)
  (declare public)
  (check-count k list-head 'index)
  (if (eq? k 0)
      '()
      (let ((head (cons (car l) '())))
        (let loop ((tail head) (l (cdr l)) (k (- k 1)))
          (if (eq? k 0)
              head
              (let ((p (cons (car l) '())))
                (set-tail! tail p)
                (loop p (cdr l) (- k 1))))))))

(define (partition ok? l)
  (declare unsafe public)
  (let ((in (cons #f '()))
        (out (cons #f '())))
    (let loop ((l l) (in-tail in) (out-tail out))
      (if (null? l)
          (values (cdr in) (cdr out))
          (let ((x (car l)))
            (if (ok? x)
                (let ((p (cons x '())))
                  (set-tail! in-tail p)
                  (loop (cdr l) p out-tail))
                (let ((p (cons x '())))
                  (set-tail! out-tail p)
                  (loop (cdr l) in-tail p))))))))

;; Stable: an element of b goes first only when strictly before
(define (merge-lists before? a b)
  (declare unsafe)
  (let ((head (cons #f '())))
    (let loop ((tail head) (a a) (b b))
      (cond
       ((null? a) (set-tail! tail b) (cdr head))
       ((null? b) (set-tail! tail a) (cdr head))
       ((before? (car b) (car a))
        (let ((p (cons (car b) '())))
          (set-tail! tail p)
          (loop p a (cdr b))))
       (else
        (let ((p (cons (car a) '())))
          (set-tail! tail p)
          (loop p (cdr a) b)))))))

;; Merge sort of the first n elements of l
(define (sort-list before? l n)
  (if (< n 2)
      (if (= n 0) '() (list (car l)))
      (let ((h (quotient n 2)))
        (merge-lists before?
                     (sort-list before? l h)
                     (sort-list before? (list-tail l h) (- n h))))))

(define (list-sort before? l)
  (declare public)
  (sort-list before? l (length l)))

(define (sort before? l)
  (declare public)
  (sort-list before? l (length l)))
