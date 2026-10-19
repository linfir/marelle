(define (tag x)
  (declare unsafe inline (result raw))
  (bit-and (as-raw x) (raw 7)))

(define (untag x)
  (declare unsafe inline (result raw))
  (bit-and (as-raw x) (raw -8)))

(define (tag=? x (raw y))
  (declare unsafe inline)
  (raw= (tag x) y))

(define (global-ref (raw i))
  (declare unsafe inline)
  (as-val (load (address "scheme_globals") i)))

(define (global-set! (raw i) v)
  (declare unsafe inline (result void))
  (store (address "scheme_globals") i v))

(define-constant procedure-type (raw (ash 42 3)))
(define-constant frame-type (raw (ash 43 3)))
(define-constant forward-type (raw (ash 44 3)))
(define-constant box-type (raw (ash 45 3)))
(define-constant bignum-type (raw (ash 46 3)))
(define-constant vector-type (raw (ash 47 3)))
;; The string types, one per kind, are 48 to 51 (lib/strings.ss)
(define-constant ascii-string-type (raw (ash 48 3)))
(define-constant utf32-string-type (raw (ash 49 3)))
;; A gensym's type differs from a symbol's by bit 3 (lib/symbols.ss)
(define-constant symbol-type (raw (ash 52 3)))
(define-constant gensym-type (raw (ash 53 3)))
(define-constant values-type (raw (ash 54 3)))
(define-constant ratnum-type (raw (ash 55 3)))
(define-constant rtd-type (raw (ash 56 3)))
(define-constant record-type (raw (ash 57 3)))
(define-constant hashtable-type (raw (ash 58 3)))
(define-constant flexvector-type (raw (ash 59 3)))
(define-constant port-type (raw (ash 60 3)))
(define-constant bytevector-type (raw (ash 61 3)))

(define (fixnum? x)
  (declare unsafe inline)
  (tag=? x (raw 1)))

(define (null? x)
  (declare unsafe inline public)
  (raw= (as-raw x) (as-raw '())))

(define (eq? x y)
  (declare unsafe inline public)
  (raw= (as-raw x) (as-raw y)))

;; eq?, except that bignums and ratnums are compared by value
(define (eqv? x y)
  (declare unsafe inline public)
  (or (raw= (as-raw x) (as-raw y))
      (and (bignum? x) (bignum? y) (= x y))
      (and (ratnum? x) (ratnum? y) (= x y))))

(define (arity-error proc (raw nargs))
  (declare unsafe noalloc (result never))
  (put-string stderr "Error: procedure of arity ")
  (let ((arity (load (as-raw proc) (raw 1))))
    (if (raw< arity (raw 0))
        (begin
          (put-string stderr "at least ")
          (put-int stderr (raw- (raw -1) arity)))
        (put-int stderr arity)))
  (put-string stderr " called with ")
  (put-int stderr nargs)
  (put-string stderr " arguments\n")
  (exit-process (raw 1)))

;; Calls: the caller allocates the callee's frame and stores the arguments in it; a call whose
;; arity differs from the callee's gets a placeholder frame instead, never entered, holding
;; its arguments for apply-frame (lib/calls.ss)

(define (procedure? x)
  (declare unsafe inline public)
  (and (tag=? x (raw 0))
       (not (raw= (as-raw x) (raw 0)))
       (raw= (load (as-raw x) (raw 0)) procedure-type)))

;; n, or -(r + 1) for r required parameters and a rest one
(define (procedure-arity proc)
  (declare unsafe inline (result raw))
  (load (as-raw proc) (raw 1)))

;; The frame length of the function at code, the word before it
(define (frame-size (raw code))
  (declare unsafe inline (result raw))
  (load code (raw -1)))

;; The frame of a call to proc, whose code is known; the caller stores the arguments
(define (make-direct-frame (raw code) proc)
  (declare unsafe inline)
  (alloc-frame (frame-size code) proc))

;; Its own frame when proc takes n arguments, else a placeholder of n arguments
(define (call-frame-size proc (raw n))
  (declare unsafe inline (result raw))
  (if (raw= (procedure-arity proc) n)
      (frame-size (load (as-raw proc) (raw 2)))
      (raw+ n (raw 6))))

;; The frame of a call to the procedure value proc with n arguments
(define (make-call-frame proc (raw n))
  (declare unsafe inline)
  (unless (procedure? proc)
    (scheme-error "expected procedure" proc))
  (alloc-frame (call-frame-size proc n) proc))

;; Whether proc is a procedure running the code at code
(define (same-code? proc (raw code))
  (declare unsafe inline)
  (and (procedure? proc)
       (raw= (load (as-raw proc) (raw 2)) code)))

;; Whether the frame f of a call with n arguments is its callee's own, not a placeholder
(define (frame-fits? f (raw n))
  (declare unsafe inline)
  (raw= (procedure-arity (as-val (load (as-raw f) (raw 5)))) n))

(define (make-box v)
  (declare unsafe inline)
  (let ((b (alloc (raw 2))))
    (store b (raw 0) box-type)
    (store b (raw 1) v)
    (as-val b)))

(define (box-ref b)
  (declare unsafe inline)
  (as-val (load (as-raw b) (raw 1))))

(define (box-set! b v)
  (declare unsafe inline (result void))
  (store (as-raw b) (raw 1) v))

;; Capture i of the current closure: only valid inlined in its lifted function
(define (closure-ref (raw i))
  (declare unsafe inline)
  (as-val (load (as-raw (self)) (raw+ i (raw 5)))))

;; A closure of proc, the template of a def with (:captures n), left to fill;
;; nothing may allocate before the captures are stored
(define (alloc-closure (raw proc))
  (declare unsafe inline (result raw))
  (let ((c (alloc (raw+ (load proc (raw 4)) (raw 5)))))
    (copy-words c proc (raw 5))
    c))

;; Sets capture i of the closure c from alloc-closure
(define (closure-set! (raw c) (raw i) v)
  (declare unsafe inline (result void))
  (store c (raw+ i (raw 5)) v))

(define (not x)
  (declare inline public)
  (if x #f #t))

;; #f is 2 and #t 10: clearing bit 3 gives 2 for them only
(define (boolean? x)
  (declare unsafe inline public)
  (raw= (bit-and (as-raw x) (raw -9)) (as-raw #f)))
