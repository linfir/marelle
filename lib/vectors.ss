;; Vectors: [vector-type, length, elem 0, ..., elem n-1], the length a raw word

(define (vector? x)
  (declare unsafe inline public)
  (and (tag=? x (raw 0))
       (not (raw= (as-raw x) (raw 0)))
       (raw= (load (as-raw x) (raw 0)) vector-type)))

(define (check-vector v who)
  (declare inline (result void))
  (unless (vector? v)
    (argument-error who 'vector v)))

;; i is a fixnum in [0, length); one unsigned compare also rejects negatives
(define (check-index v i who)
  (declare unsafe inline (result void))
  (check-vector v who)
  (unless (and (fixnum? i) (raw<u (fixnum-value i) (load (as-raw v) (raw 1))))
    (index-error who i v)))

(define (vector-length v)
  (declare unsafe inline public)
  (check-vector v vector-length)
  (make-fixnum (load (as-raw v) (raw 1))))

(define (vector-ref v i)
  (declare unsafe inline public)
  (check-index v i vector-ref)
  (as-val (load (as-raw v) (raw+ (fixnum-value i) (raw 2)))))

(define (vector-set! v i x)
  (declare unsafe inline public)
  (check-index v i vector-set!)
  (store (as-raw v) (raw+ (fixnum-value i) (raw 2)) x)
  undef)

(define (make-vector k fill)
  (declare unsafe inline public)
  (unless (and (fixnum? k) (raw>= (fixnum-value k) (raw 0)))
    (argument-error make-vector 'vector-length k))
  (let ((p (alloc (raw+ (fixnum-value k) (raw 2)))))
    (store p (raw 0) vector-type)
    (store p (raw 1) (fixnum-value k))
    (fill-words (raw+ p (raw 16)) (fixnum-value k) (as-raw fill))
    (as-val p)))

(define (vector-fill! v x)
  (declare unsafe public)
  (unless (vector? v)
    (argument-error vector-fill! 'mutable-vector v))
  (fill-words (raw+ (as-raw v) (raw 16)) (load (as-raw v) (raw 1)) (as-raw x))
  undef)

(define (vector->list v)
  (declare public)
  (let loop ((i (vector-length v))
             (acc '()))
    (if (= i 0)
        acc
        (loop (- i 1) (cons (vector-ref v (- i 1)) acc)))))

(define (list->vector l)
  (declare public)
  (let ((v (make-vector (length l) 0)))
    (let loop ((l l)
               (i 0))
      (if (null? l)
          v
          (begin
            (vector-set! v i (car l))
            (loop (cdr l) (+ i 1)))))))

(define (vector . xs)
  (declare public)
  (list->vector xs))

(define (variadic-make-vector k . fill)
  (cond
   ((null? fill) (make-vector k 0))
   ((null? (cdr fill)) (make-vector k (car fill)))
   (else (argument-count-error make-vector (+ 1 (length fill))))))

(define (vector-copy v)
  (declare unsafe public)
  (check-vector v vector-copy)
  (let ((c (make-vector (vector-length v) 0)))
    (copy-words (raw+ (as-raw c) (raw 16)) (raw+ (as-raw v) (raw 16)) (load (as-raw v) (raw 1)))
    c))

(define (vector-for-each f v)
  (declare public)
  (let loop ((i 0))
    (when (< i (vector-length v))
      (f (vector-ref v i))
      (loop (+ i 1)))))

;; Up to the shortest vector
(define (variadic-vector-for-each f v . vs)
  (let ((vs (cons v vs)))
    (let ((n (apply min (map vector-length vs))))
      (let loop ((i 0))
        (when (< i n)
          (apply f (map (lambda (v) (vector-ref v i)) vs))
          (loop (+ i 1)))))))
