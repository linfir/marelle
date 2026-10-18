;; Flexvectors (SRFI 214): [flexvector-type, length, storage], the length a raw word; the
;; storage is a vector of capacity >= 4 and >= length, its slots past the length 0

(define (flexvector? x)
  (declare unsafe inline public)
  (and (tag=? x (raw 0))
       (not (raw= (as-raw x) (raw 0)))
       (raw= (load (as-raw x) (raw 0)) (flexvector-type))))

(define (check-flexvector fv (raw msg))
  (declare unsafe inline (result void))
  (unless (flexvector? fv)
    (scheme-error msg fv)))

(define (check-flex-index fv i (raw msg) (raw range-msg))
  (declare unsafe inline (result void))
  (check-flexvector fv msg)
  (unless (and (fixnum? i) (raw<u (fixnum-value i) (load (as-raw fv) (raw 1))))
    (scheme-error range-msg i)))

;; The address of element i of the storage
(define (flex-slot fv (raw i))
  (declare unsafe inline (result raw))
  (raw+ (load (as-raw fv) (raw 2)) (raw<< (raw+ i (raw 2)) (raw 3))))

;; An empty flexvector with room for k elements, a fixnum
(define (alloc-flexvector k)
  (declare unsafe inline)
  (let* ((s (make-vector (if (< k 4) 4 k) 0))
         (p (alloc (raw 3))))
    (store p (raw 0) (flexvector-type))
    (store p (raw 2) s)
    (as-val p)))

(define (flexvector-length fv)
  (declare unsafe inline public)
  (check-flexvector fv "`flexvector-length` expected a flexvector")
  (make-fixnum (load (as-raw fv) (raw 1))))

(define (flexvector-ref fv i)
  (declare unsafe inline public)
  (check-flex-index fv i "`flexvector-ref` expected a flexvector" "`flexvector-ref` index out of range")
  (as-val (load (flex-slot fv (fixnum-value i)) (raw 0))))

;; Returns the previous value; an index equal to the length appends x
(define (flexvector-set! fv i x)
  (declare unsafe inline public)
  (if (and (flexvector? fv) (fixnum? i) (raw<u (fixnum-value i) (load (as-raw fv) (raw 1))))
      (let ((old (as-val (load (flex-slot fv (fixnum-value i)) (raw 0)))))
        (store (flex-slot fv (fixnum-value i)) (raw 0) x)
        old)
      (flexvector-set-end! fv i x)))

(define (flexvector-set-end! fv i x)
  (declare unsafe)
  (check-flexvector fv "`flexvector-set!` expected a flexvector")
  (unless (and (fixnum? i) (raw= (fixnum-value i) (load (as-raw fv) (raw 1))))
    (scheme-error "`flexvector-set!` index out of range" i))
  (flexvector-add-back! fv x)
  undef)

;; Doubles the storage
(define (flexvector-grow! fv)
  (declare unsafe)
  (let ((s (make-vector (make-fixnum (raw<< (load (load (as-raw fv) (raw 2)) (raw 1)) (raw 1))) 0)))
    (copy-words (raw+ (as-raw s) (raw 16)) (flex-slot fv (raw 0)) (load (as-raw fv) (raw 1)))
    (store (as-raw fv) (raw 2) s)
    undef))

(define (flexvector-add-back! fv x)
  (declare unsafe inline public)
  (check-flexvector fv "`flexvector-add-back!` expected a flexvector")
  (when (raw= (load (as-raw fv) (raw 1)) (load (load (as-raw fv) (raw 2)) (raw 1)))
    (flexvector-grow! fv))
  (let ((n (load (as-raw fv) (raw 1))))
    (store (flex-slot fv n) (raw 0) x)
    (store (as-raw fv) (raw 1) (raw+ n (raw 1)))
    fv))

(define (variadic-flexvector-add-back! fv . xs)
  (declare unsafe)
  (check-flexvector fv "`flexvector-add-back!` expected a flexvector")
  (let loop ((xs xs))
    (if (null? xs)
        fv
        (begin
          (flexvector-add-back! fv (car xs))
          (loop (cdr xs))))))

;; The last element, its slot cleared so that it keeps nothing alive
(define (flexvector-remove-back! fv)
  (declare unsafe inline public)
  (check-flexvector fv "`flexvector-remove-back!` expected a flexvector")
  (let ((n (raw- (load (as-raw fv) (raw 1)) (raw 1))))
    (when (raw< n (raw 0))
      (scheme-error "`flexvector-remove-back!` expected a nonempty flexvector" fv))
    (let ((x (as-val (load (flex-slot fv n) (raw 0)))))
      (store (flex-slot fv n) (raw 0) (raw 0))
      (store (as-raw fv) (raw 1) n)
      x)))

(define (flexvector-clear! fv)
  (declare unsafe public)
  (check-flexvector fv "`flexvector-clear!` expected a flexvector")
  (fill-words (flex-slot fv (raw 0)) (load (as-raw fv) (raw 1)) (raw 0))
  (store (as-raw fv) (raw 1) (raw 0))
  fv)

(define (make-flexvector k . fill)
  (declare unsafe public)
  (unless (and (fixnum? k) (raw>= (fixnum-value k) (raw 0)))
    (scheme-error "`make-flexvector` expected a nonnegative fixnum" k))
  (unless (or (null? fill) (null? (cdr fill)))
    (scheme-error "`make-flexvector` expected 1 or 2 arguments" fill))
  (let ((x (if (null? fill) 0 (car fill)))
        (fv (alloc-flexvector k)))
    (fill-words (flex-slot fv (raw 0)) (fixnum-value k) (as-raw x))
    (store (as-raw fv) (raw 1) (fixnum-value k))
    fv))

(define (flexvector->vector fv)
  (declare unsafe public)
  (check-flexvector fv "`flexvector->vector` expected a flexvector")
  (let ((v (make-vector (make-fixnum (load (as-raw fv) (raw 1))) 0)))
    (copy-words (raw+ (as-raw v) (raw 16)) (flex-slot fv (raw 0)) (load (as-raw fv) (raw 1)))
    v))

(define (vector->flexvector v)
  (declare unsafe public)
  (check-vector v "`vector->flexvector` expected a vector")
  (let ((fv (alloc-flexvector (make-fixnum (load (as-raw v) (raw 1))))))
    (copy-words (flex-slot fv (raw 0)) (raw+ (as-raw v) (raw 16)) (load (as-raw v) (raw 1)))
    (store (as-raw fv) (raw 1) (load (as-raw v) (raw 1)))
    fv))

(define (flexvector->list fv)
  (declare public)
  (let loop ((i (flexvector-length fv))
             (acc '()))
    (if (= i 0)
        acc
        (loop (- i 1) (cons (flexvector-ref fv (- i 1)) acc)))))

(define (list->flexvector l)
  (declare public)
  (vector->flexvector (list->vector l)))

(define (flexvector . xs)
  (declare public)
  (list->flexvector xs))
