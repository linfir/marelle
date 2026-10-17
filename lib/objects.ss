(define (tag x)
  (declare inline (result raw))
  (bit-and (as-raw x) (raw 7)))

(define (untag x)
  (declare inline (result raw))
  (bit-and (as-raw x) (raw -8)))

(define (tag=? x (raw y))
  (declare inline)
  (raw= (tag x) y))

(define (global-ref (raw i))
  (declare inline)
  (as-val (load (address "scheme_globals") i)))

(define (global-set! (raw i) v)
  (declare inline (result void))
  (store (address "scheme_globals") i v))

(define (procedure-type)
  (declare inline (result raw))
  (raw 336)) ; 42 << 3

(define (frame-type)
  (declare inline (result raw))
  (raw 344)) ; 43 << 3

(define (forward-type)
  (declare inline (result raw))
  (raw 352)) ; 44 << 3

(define (box-type)
  (declare inline (result raw))
  (raw 360)) ; 45 << 3

(define (bignum-type)
  (declare inline (result raw))
  (raw 368)) ; 46 << 3

(define (fixnum? x)
  (declare inline)
  (tag=? x (raw 1)))

(define (null? x)
  (declare inline public)
  (raw= (as-raw x) (as-raw '())))

(define (eq? x y)
  (declare inline public)
  (raw= (as-raw x) (as-raw y)))

(define (arity-error proc (raw nargs))
  (declare native (result never))
  (put-string (stderr) "Error: procedure of arity ")
  (put-int (stderr) (load (as-raw proc) (raw 1)))
  (put-string (stderr) " called with ")
  (put-int (stderr) nargs)
  (put-string (stderr) " arguments\n")
  (exit-process (raw 1)))

;; Checks that proc is a procedure taking nargs arguments
(define (check-procedure proc (raw nargs))
  (declare native (result void))
  (unless (and (tag=? proc (raw 0))
               (not (raw= (as-raw proc) (raw 0)))
               (raw= (load (as-raw proc) (raw 0)) (procedure-type)))
    (scheme-error "expected procedure" proc))
  (unless (raw= (load (as-raw proc) (raw 1)) nargs)
    (arity-error proc nargs)))

(define (make-box v)
  (declare inline)
  (let ((b (alloc (raw 2))))
    (store b (raw 0) (box-type))
    (store b (raw 1) v)
    (as-val b)))

(define (box-ref b)
  (declare inline)
  (as-val (load (as-raw b) (raw 1))))

(define (box-set! b v)
  (declare inline (result void))
  (store (as-raw b) (raw 1) v))

;; Capture i of the current closure: only valid inlined in its lifted function
(define (closure-ref (raw i))
  (declare inline)
  (as-val (load (as-raw (self)) (raw+ i (raw 5)))))

;; A closure of proc, the template of a def with (:captures n), left to fill;
;; nothing may allocate before the captures are stored
(define (alloc-closure (raw proc))
  (declare inline (result raw))
  (let ((c (alloc (raw+ (load proc (raw 4)) (raw 5)))))
    (copy-words c proc (raw 5))
    c))

;; Sets capture i of the closure c from alloc-closure
(define (closure-set! (raw c) (raw i) v)
  (declare inline (result void))
  (store c (raw+ i (raw 5)) v))

(define (not x)
  (declare inline public)
  (if x #f #t))
