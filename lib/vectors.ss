;; Vectors: [vector-type, length, elem 0, ..., elem n-1], the length a raw word

(define (vector? x)
  (declare unsafe inline public)
  (and (tag=? x (raw 0))
       (not (raw= (as-raw x) (raw 0)))
       (raw= (load (as-raw x) (raw 0)) vector-type)))

(define (check-vector v (raw msg))
  (declare unsafe inline (result void))
  (unless (vector? v)
    (scheme-error msg v)))

;; i is a fixnum in [0, length); one unsigned compare also rejects negatives
(define (check-index v i (raw msg) (raw range-msg))
  (declare unsafe inline (result void))
  (check-vector v msg)
  (unless (and (fixnum? i) (raw<u (fixnum-value i) (load (as-raw v) (raw 1))))
    (scheme-error range-msg i)))

(define (vector-length v)
  (declare unsafe inline public)
  (check-vector v "`vector-length` expected a vector")
  (make-fixnum (load (as-raw v) (raw 1))))

(define (vector-ref v i)
  (declare unsafe inline public)
  (check-index v i "`vector-ref` expected a vector" "`vector-ref` index out of range")
  (as-val (load (as-raw v) (raw+ (fixnum-value i) (raw 2)))))

(define (vector-set! v i x)
  (declare unsafe inline public)
  (check-index v i "`vector-set!` expected a vector" "`vector-set!` index out of range")
  (store (as-raw v) (raw+ (fixnum-value i) (raw 2)) x)
  undef)

(define (make-vector k fill)
  (declare unsafe inline public)
  (unless (and (fixnum? k) (raw>= (fixnum-value k) (raw 0)))
    (scheme-error "`make-vector` expected a nonnegative fixnum" k))
  (let ((p (alloc (raw+ (fixnum-value k) (raw 2)))))
    (store p (raw 0) vector-type)
    (store p (raw 1) (fixnum-value k))
    (fill-words (raw+ p (raw 16)) (fixnum-value k) (as-raw fill))
    (as-val p)))

(define (vector-fill! v x)
  (declare unsafe public)
  (check-vector v "`vector-fill!` expected a vector")
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
  (declare unsafe)
  (cond
   ((null? fill) (make-vector k 0))
   ((null? (cdr fill)) (make-vector k (car fill)))
   (else (scheme-error "`make-vector` expected 1 or 2 arguments" fill))))
