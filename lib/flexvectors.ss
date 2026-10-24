;; Flexvectors (SRFI 214): [flexvector-type, length, capacity, elems...], the length and the
;; capacity raw words, capacity >= 4 and >= length, the slots past the length 0. A full one grows
;; by relocating to a copy of twice the capacity (relocate!, lib/objects.ss).

(define (flexvector? x)
  (declare unsafe inline public)
  (or (header=? x flexvector-type)
      (moved-to? x flexvector-type)))

(define (check-flexvector fv who)
  (declare inline (result void))
  (unless (flexvector? fv)
    (argument-error who 'flexvector fv)))

;; The body of the flexvector fv, checked
(define (flexvector-body fv who)
  (declare unsafe inline)
  (if (header=? fv flexvector-type)
      fv
      (or (relocated-body fv flexvector-type)
          (argument-error who 'flexvector fv))))

(define (flex-length b)
  (declare unsafe inline (result raw))
  (load (as-raw b) (raw 1)))

(define (flex-capacity b)
  (declare unsafe inline (result raw))
  (load (as-raw b) (raw 2)))

;; The address of element i of the body b
(define (flex-slot b (raw i))
  (declare unsafe inline (result raw))
  (raw+ (as-raw b) (raw<< (raw+ i (raw 3)) (raw 3))))

;; The body b of a flexvector, after checking the index i, a fixnum in [0, length)
(define (flex-index-body fv i who)
  (declare unsafe inline)
  (let ((b (flexvector-body fv who)))
    (unless (and (fixnum? i) (raw<u (fixnum-value i) (flex-length b)))
      (index-error who i fv))
    b))

;; An empty body with room for k elements, a fixnum
(define (alloc-flexvector k)
  (declare unsafe inline)
  (let* ((cap (if (< k 4) 4 k))
         (p (alloc (raw+ (fixnum-value cap) (raw 3)))))
    (store p (raw 0) flexvector-type)
    (store p (raw 2) (fixnum-value cap))
    (as-val p)))

(define (flexvector-length fv)
  (declare unsafe inline public)
  (make-fixnum (flex-length (flexvector-body fv flexvector-length))))

(define (flexvector-ref fv i)
  (declare unsafe inline public)
  (as-val (load (flex-slot (flex-index-body fv i flexvector-ref) (fixnum-value i)) (raw 0))))

;; Returns the previous value; an index equal to the length appends x
(define (flexvector-set! fv i x)
  (declare unsafe inline public)
  (let ((b (flexvector-body fv flexvector-set!)))
    (if (and (fixnum? i) (raw<u (fixnum-value i) (flex-length b)))
        (let ((old (as-val (load (flex-slot b (fixnum-value i)) (raw 0)))))
          (store (flex-slot b (fixnum-value i)) (raw 0) x)
          old)
        (flexvector-set-end! fv i x))))

(define (flexvector-set-end! fv i x)
  (declare unsafe)
  (let ((b (flexvector-body fv flexvector-set!)))
    (unless (and (fixnum? i) (raw= (fixnum-value i) (flex-length b)))
      (index-error flexvector-set! i fv)))
  (flexvector-add-back! fv x)
  undef)

;; The new body of the full body b: twice the capacity, b relocated to it
(define (flexvector-grow! b)
  (declare unsafe)
  (let ((p (alloc (raw+ (raw<< (flex-capacity b) (raw 1)) (raw 3)))))
    (store p (raw 0) flexvector-type)
    (store p (raw 1) (flex-length b))
    (store p (raw 2) (raw<< (flex-capacity b) (raw 1)))
    (copy-words (raw+ p (raw 24)) (flex-slot b (raw 0)) (flex-length b))
    (relocate! b (as-val p))
    (as-val p)))

(define flexvector-add-back!
  (case-lambda
   (declare public)
   ((fv) (check-flexvector fv flexvector-add-back!) fv)
   ((fv x)
    (declare unsafe inline)
    (let* ((b (flexvector-body fv flexvector-add-back!))
           (b (if (raw= (flex-length b) (flex-capacity b)) (flexvector-grow! b) b))
           (n (flex-length b)))
      (store (flex-slot b n) (raw 0) x)
      (store (as-raw b) (raw 1) (raw+ n (raw 1)))
      fv))
   ((fv x y . rest)
    (flexvector-add-back! fv x)
    (apply flexvector-add-back! fv y rest))))

;; The last element, its slot cleared so that it keeps nothing alive
(define (flexvector-remove-back! fv)
  (declare unsafe inline public)
  (let* ((b (flexvector-body fv flexvector-remove-back!))
         (n (raw- (flex-length b) (raw 1))))
    (when (raw< n (raw 0))
      (argument-error flexvector-remove-back! 'nonempty-flexvector fv))
    (let ((x (as-val (load (flex-slot b n) (raw 0)))))
      (store (flex-slot b n) (raw 0) (raw 0))
      (store (as-raw b) (raw 1) n)
      x)))

(define (flexvector-clear! fv)
  (declare unsafe public)
  (let ((b (flexvector-body fv flexvector-clear!)))
    (fill-words (flex-slot b (raw 0)) (flex-length b) (raw 0))
    (store (as-raw b) (raw 1) (raw 0))
    fv))

(define (make-flexvector k . fill)
  (declare unsafe public)
  (unless (and (fixnum? k) (raw>= (fixnum-value k) (raw 0)))
    (argument-error make-flexvector 'size k))
  (unless (or (null? fill) (null? (cdr fill)))
    (argument-count-error make-flexvector (+ 1 (length fill))))
  (let ((x (if (null? fill) 0 (car fill)))
        (fv (alloc-flexvector k)))
    (fill-words (flex-slot fv (raw 0)) (fixnum-value k) (as-raw x))
    (store (as-raw fv) (raw 1) (fixnum-value k))
    fv))

(define (flexvector->vector fv)
  (declare unsafe public)
  (let* ((b (flexvector-body fv flexvector->vector))
         (v (make-vector (make-fixnum (flex-length b)) 0)))
    (copy-words (raw+ (as-raw v) (raw 16)) (flex-slot b (raw 0)) (flex-length b))
    v))

(define (vector->flexvector v)
  (declare unsafe public)
  (check-vector v vector->flexvector)
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
