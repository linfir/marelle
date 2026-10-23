;; The low 3 bits of a value: 0 an object with a header, 1 a fixnum, 2 another immediate, 3 a
;; pair, 7 an immutable pair
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

;; A relocated object: [moved-type, its new body] (relocate!)
(define-constant moved-type (raw (ash 41 3)))
(define-constant procedure-type (raw (ash 42 3)))
(define-constant frame-type (raw (ash 43 3)))
(define-constant forward-type (raw (ash 44 3)))
(define-constant box-type (raw (ash 45 3)))
(define-constant bignum-type (raw (ash 46 3)))
(define-constant vector-type (raw (ash 47 3)))
;; The string types, one per kind, are 48 to 51 (lib/strings.ss)
(define-constant ascii-string-type (raw (ash 48 3)))
(define-constant utf32-string-type (raw (ash 49 3)))
(define-constant utf8-string-type (raw (ash 50 3)))
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
;; An immutable bytevector's type differs from bytevector-type by bit 4 (lib/bytevectors.ss)
(define-constant immutable-bytevector-type (raw (ash 63 3)))
;; An immutable vector's or string's type is the mutable one's plus 64, header bit 9: static
;; ones, and literals frozen at startup (lib/vectors.ss, lib/strings.ss)
(define-constant immutable-vector-type (raw (ash 111 3)))
(define-constant immutable-ascii-string-type (raw (ash 112 3)))
(define-constant immutable-utf8-string-type (raw (ash 114 3)))

(define (fixnum? x)
  (declare unsafe inline public)
  (tag=? x (raw 1)))

(define (null? x)
  (declare unsafe inline public)
  (raw= (as-raw x) (as-raw '())))

(define (eq? x y)
  (declare unsafe inline public)
  (raw= (as-raw x) (as-raw y)))

;; Relocation: an object that must grow or change its layout keeps its identity by becoming
;; [moved-type, new body]; every access follows the chain, and the next collection redirects
;; every pointer to the body. A procedure resolves its argument once, after the type check, and
;; never returns or stores the body; it resolves again after a call into Scheme, which may
;; relocate the object. Relocating allocates, so noalloc code never sees it happen. Pairs and
;; static objects never move.

(define (moved? x)
  (declare unsafe inline)
  (and (tag=? x (raw 0))
       (not (raw= (as-raw x) (raw 0)))
       (raw= (load (as-raw x) (raw 0)) moved-type)))

;; The current body of x, x itself unless relocated; x then forwards to it directly, so a
;; reference held across several relocations follows one link
(define (resolve x)
  (declare unsafe noalloc)
  (if (moved? x)
      (let follow ((y (as-val (load (as-raw x) (raw 1)))))
        (if (moved? y)
            (follow (as-val (load (as-raw y) (raw 1))))
            (begin
              (store (as-raw x) (raw 1) y)
              y)))
      x))

;; The body of x if it was relocated to an object of header h, else #f
(define (moved-body x (raw h))
  (declare unsafe noalloc)
  (and (moved? x)
       (let ((b (resolve x)))
         (and (raw= (load (as-raw b) (raw 0)) h) b))))

;; Whether x points to an object of header h
(define (header=? x (raw h))
  (declare unsafe inline)
  (and (tag=? x (raw 0))
       (not (raw= (as-raw x) (raw 0)))
       (raw= (load (as-raw x) (raw 0)) h)))

;; moved-body, one link followed inline, so a reference relocated once costs two loads
(define (relocated-body x (raw h))
  (declare unsafe inline)
  (if (and (header=? x moved-type)
           (raw= (load (load (as-raw x) (raw 1)) (raw 0)) h))
      (as-val (load (as-raw x) (raw 1)))
      (moved-body x h)))

;; The body of x if it has the header h, or was relocated to an object of header h, else #f;
;; the relocated case only costs on the failing path of the header check
(define (body-of x (raw h))
  (declare unsafe inline)
  (if (header=? x h) x (relocated-body x h)))

;; Whether x, relocated, now has the header h
(define (moved-to? x (raw h))
  (declare unsafe noalloc)
  (if (moved-body x h) #t #f))

;; Makes the body old, resolved, forward to new
(define (relocate! old new)
  (declare unsafe inline (result void))
  (store (as-raw old) (raw 0) moved-type)
  (store (as-raw old) (raw 1) new))

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

;; Calls: the caller allocates the callee's frame in the frame region and stores the arguments
;; in it; a call whose arity differs from the callee's gets a placeholder frame on the heap
;; instead, never entered, holding its arguments for apply-frame (lib/calls.ss)

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

;; The frame of a call to the procedure value proc with n arguments: its own when proc takes n
;; arguments, else a placeholder of n arguments
(define (make-call-frame proc (raw n))
  (declare unsafe inline)
  (unless (procedure? proc)
    (non-procedure-error proc))
  (if (raw= (procedure-arity proc) n)
      (alloc-frame (frame-size (load (as-raw proc) (raw 2))) proc)
      (alloc-placeholder (raw+ n (raw 6)) proc)))

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

;; Chez's unspecified value
(define (void)
  (declare unsafe inline public)
  undef)

(define (not x)
  (declare inline public)
  (if x #f #t))

;; #f is 2 and #t 10: clearing bit 3 gives 2 for them only
(define (boolean? x)
  (declare unsafe inline public)
  (raw= (bit-and (as-raw x) (raw -9)) (as-raw #f)))
