;; Calls whose arity differs from the callee's go through a placeholder frame holding their
;; n arguments; a variadic callee, of arity word -(r + 1), takes the first r and a list of the
;; others

(define (frame-argument-count f)
  (declare unsafe inline (result raw))
  (raw- (load (as-raw f) (raw 1)) (raw 6)))

(define (frame-argument f i)
  (declare unsafe inline)
  (as-val (load (as-raw f) (raw+ (fixnum-value i) (raw 6)))))

;; The required parameters of a variadic procedure, or -1
(define (required-count proc)
  (declare unsafe inline)
  (if (raw< (procedure-arity proc) (raw 0))
      (make-fixnum (raw- (raw -1) (procedure-arity proc)))
      -1))

;; The fresh list of the arguments i and after of the placeholder frame f
(define (frame-rest f i)
  (declare unsafe)
  (let next ((j (make-fixnum (frame-argument-count f)))
             (acc '()))
    (if (= j i)
        acc
        (next (- j 1) (cons (frame-argument f (- j 1)) acc)))))

;; Calls the callee of the placeholder frame f with its arguments
(define (apply-frame f)
  (declare unsafe)
  (let* ((proc (as-val (load (as-raw f) (raw 5))))
         (r (required-count proc)))
    (when (or (< r 0) (raw< (frame-argument-count f) (fixnum-value r)))
      (arity-error proc (frame-argument-count f)))
    (let* ((rest (frame-rest f r))
           (g (make-direct-frame (load (as-raw proc) (raw 2)) proc)))
      (copy-words (raw+ (as-raw g) (raw 48)) (raw+ (as-raw f) (raw 48)) (fixnum-value r))
      (store (as-raw g) (raw+ (fixnum-value r) (raw 6)) rest)
      (tail-call-frame g))))

;; Stores the elements of the list l as the arguments of the frame f
(define (store-arguments! (raw f) l)
  (declare unsafe noalloc (result void))
  (let next ((l l)
             (i (raw 6)))
    (if (pair? l)
        (begin
          (store f i (car l))
          (next (cdr l) (raw+ i (raw 1))))
        undef)))

;; The length of the proper list l, else an error
(define (argument-count l)
  (declare unsafe)
  (let next ((x l)
             (n 0))
    (cond
     ((null? x) n)
     ((pair? x) (next (cdr x) (+ n 1)))
     (else (scheme-error "`apply` expected a list" l)))))

;; x rest ... last as one list, last shared, and checked
(define (spread x rest)
  (if (null? rest)
      (begin
        (argument-count x)
        x)
      (cons x (spread (car rest) (cdr rest)))))

(define (apply f x . rest)
  (declare unsafe public)
  (let* ((args (spread x rest))
         (n (argument-count args))
         (g (make-call-frame f (fixnum-value n))))
    (store-arguments! (as-raw g) args)
    (if (frame-fits? g (fixnum-value n))
        (tail-call-frame g)
        (apply-frame g))))

;; Multiple values are [values-type, list], except one value, which is itself
(define (values x)
  (declare inline public)
  x)

(define (variadic-values . xs)
  (declare unsafe)
  (if (and (pair? xs) (null? (cdr xs)))
      (car xs)
      (let ((p (alloc (raw 2))))
        (store p (raw 0) values-type)
        (store p (raw 1) xs)
        (as-val p))))

(define (multiple-values? x)
  (declare unsafe inline)
  (and (tag=? x (raw 0))
       (not (raw= (as-raw x) (raw 0)))
       (raw= (load (as-raw x) (raw 0)) values-type)))

(define (call-with-values producer consumer)
  (declare unsafe public)
  (let ((x (producer)))
    (if (multiple-values? x)
        (apply consumer (as-val (load (as-raw x) (raw 1))))
        (consumer x))))
