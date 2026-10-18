;; The subset of SRFI 214 in lib/flexvectors.ss, for comparing the tests with Chez
(define-record-type flexvector-record
  (fields (mutable length) (mutable storage)))

(define (flexvector? x)
  (flexvector-record? x))

(define (flexvector-length fv)
  (flexvector-record-length fv))

(define (check-flex-index fv i)
  (unless (and (fixnum? i) (<= 0 i) (< i (flexvector-record-length fv)))
    (error 'flexvector "index out of range" i)))

(define (flexvector-ref fv i)
  (check-flex-index fv i)
  (vector-ref (flexvector-record-storage fv) i))

(define (flexvector-set! fv i x)
  (if (eqv? i (flexvector-record-length fv))
      (begin (flexvector-add-back! fv x) (void))
      (let ((old (flexvector-ref fv i)))
        (vector-set! (flexvector-record-storage fv) i x)
        old)))

(define (flexvector-add-back! fv . xs)
  (for-each (lambda (x)
              (let ((n (flexvector-record-length fv))
                    (s (flexvector-record-storage fv)))
                (when (= n (vector-length s))
                  (let ((new (make-vector (* 2 n) 0)))
                    (vector-for-each (lambda (i) (vector-set! new i (vector-ref s i))) (list->vector (iota n)))
                    (flexvector-record-storage-set! fv new)))
                (vector-set! (flexvector-record-storage fv) n x)
                (flexvector-record-length-set! fv (+ n 1))))
            xs)
  fv)

(define (flexvector-remove-back! fv)
  (let ((n (flexvector-record-length fv)))
    (when (= n 0)
      (error 'flexvector-remove-back! "empty flexvector" fv))
    (let ((x (flexvector-ref fv (- n 1))))
      (flexvector-record-length-set! fv (- n 1))
      x)))

(define (flexvector-clear! fv)
  (flexvector-record-length-set! fv 0)
  fv)

(define (make-flexvector k . fill)
  (let ((fv (make-flexvector-record 0 (make-vector (max k 4) 0))))
    (for-each (lambda (i) (flexvector-add-back! fv (if (null? fill) 0 (car fill)))) (iota k))
    fv))

(define (flexvector->vector fv)
  (vector-copy (flexvector-record-storage fv) 0 (flexvector-record-length fv)))

(define (flexvector->list fv)
  (vector->list (flexvector->vector fv)))

(define (list->flexvector l)
  (apply flexvector-add-back! (make-flexvector 0) l))

(define (vector->flexvector v)
  (list->flexvector (vector->list v)))

(define (flexvector . xs)
  (list->flexvector xs))
