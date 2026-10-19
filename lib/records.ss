;; Records, for the define-record-type of the front pass: a type is [rtd-type, name], made when
;; the form is evaluated; a record is [record-type, n, type, field 0, ..., field n-1], n raw

(define (make-record-type name)
  (declare unsafe)
  (let ((t (alloc (raw 2))))
    (store t (raw 0) rtd-type)
    (store t (raw 1) name)
    (as-val t)))

(define (make-record type n)
  (declare unsafe inline)
  (let ((r (alloc (raw+ (fixnum-value n) (raw 3)))))
    (store r (raw 0) record-type)
    (store r (raw 1) (fixnum-value n))
    (store r (raw 2) type)
    (as-val r)))

(define (record-of? x type)
  (declare unsafe inline)
  (and (tag=? x (raw 0))
       (not (raw= (as-raw x) (raw 0)))
       (raw= (load (as-raw x) (raw 0)) record-type)
       (raw= (load (as-raw x) (raw 2)) (as-raw type))))

;; Unchecked: the callers test record-of? first
(define (record-field r i)
  (declare unsafe inline)
  (as-val (load (as-raw r) (raw+ (fixnum-value i) (raw 3)))))

(define (record-field-set! r i x)
  (declare unsafe inline)
  (store (as-raw r) (raw+ (fixnum-value i) (raw 3)) x)
  undef)

(define (record-error who type x)
  (declare unsafe noalloc (result never))
  (put-string stderr "Error: `")
  (display-symbol stderr who)
  (put-string stderr "` expected a record of type ")
  (display-symbol stderr (as-val (load (as-raw type) (raw 1))))
  (put-string stderr ": ")
  (display stderr x)
  (put-string stderr "\n")
  (exit-process (raw 1)))
