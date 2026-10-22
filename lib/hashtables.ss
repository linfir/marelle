;; Hashtables (R6RS): [hashtable-type, count, naddr, epoch, shift, kind, hash, equiv, slots...].
;; count, naddr (the keys hashed by address), epoch (gc-count when they were placed), shift
;; (64 - k for 2^k slots) and kind are raw words; the 2^k slots follow, a key and a value each, a
;; key 0 when empty, at most half full; hash and equiv are the procedures of a generic table,
;; else #f. A resize builds a new body and relocates the table to it (relocate!,
;; lib/objects.ss); the procedures here take the body. The hash and equiv procedures may
;; relocate the table, so the generic code resolves it after each call.
;; Linear probing from the top k bits of the mixed hash, as for symbols.
;; A collection moves the keys hashed by address, so the next lookup places them again.

(define-constant eq-kind (raw 0))
(define-constant eqv-kind (raw 1))
(define-constant generic-kind (raw 2))
(define-constant golden (raw 11400714819323198485)) ; 0x9E3779B97F4A7C15

(define (hashtable? x)
  (declare unsafe inline public)
  (or (header=? x hashtable-type)
      (moved-to? x hashtable-type)))

;; The body of the hashtable t, checked
(define (hashtable-body t who)
  (declare unsafe inline)
  (if (header=? t hashtable-type)
      t
      (or (relocated-body t hashtable-type)
          (argument-error who 'hashtable t))))

(define (table-count t)
  (declare unsafe inline (result raw))
  (load (as-raw t) (raw 1)))

(define (table-kind t)
  (declare unsafe inline (result raw))
  (load (as-raw t) (raw 5)))

(define (table-hash t)
  (declare unsafe inline)
  (as-val (load (as-raw t) (raw 6))))

(define (table-equiv t)
  (declare unsafe inline)
  (as-val (load (as-raw t) (raw 7))))

;; The body itself, which holds the slots
(define (table-storage t)
  (declare unsafe inline (result raw))
  (as-raw t))

(define (storage-slots (raw s))
  (declare unsafe inline (result raw))
  (raw<< (raw 1) (raw- (raw 64) (load s (raw 4)))))

;; Word j of slot i of the body s: 0 the key, 1 the value
(define (slot-word (raw s) (raw i) (raw j))
  (declare unsafe inline (result raw))
  (load s (raw+ (raw<< i (raw 1)) (raw+ j (raw 8)))))

(define (set-slot! (raw s) (raw i) k v)
  (declare unsafe inline (result void))
  (store s (raw+ (raw<< i (raw 1)) (raw 8)) k)
  (store s (raw+ (raw<< i (raw 1)) (raw 9)) v))

(define (clear-slot! (raw s) (raw i))
  (declare unsafe inline (result void))
  (store s (raw+ (raw<< i (raw 1)) (raw 8)) (raw 0))
  (store s (raw+ (raw<< i (raw 1)) (raw 9)) (raw 0)))

;; The fixnum slot i of t
(define (slot-empty? t i)
  (declare unsafe inline)
  (raw= (slot-word (table-storage t) (fixnum-value i) (raw 0)) (raw 0)))

(define (next-slot (raw s) (raw i))
  (declare unsafe inline (result raw))
  (bit-and (raw+ i (raw 1)) (raw- (storage-slots s) (raw 1))))

(define (home-slot (raw s) (raw h))
  (declare unsafe inline (result raw))
  (raw>>u (raw* h golden) (load s (raw 4))))

(define (mix (raw h) (raw x))
  (declare unsafe inline (result raw))
  (raw* (raw+ h (raw+ x (raw 1))) golden))

(define (hash-fixnum (raw h))
  (declare unsafe inline)
  (make-fixnum (raw>>u h (raw 4))))

;; Bignums by sign and limbs, ratnums by numerator and denominator, fixnums by word
(define (number-hash x)
  (declare unsafe noalloc (result raw))
  (cond
   ((bignum? x)
    (let limb-next ((i (raw 0))
                    (h (load (as-raw x) (raw 2))))
      (if (raw= i (load (as-raw x) (raw 1)))
          h
          (limb-next (raw+ i (raw 1)) (mix h (load (as-raw x) (raw+ i (raw 3))))))))
   ((ratnum? x)
    (mix (number-hash (as-val (load (as-raw x) (raw 1))))
         (number-hash (as-val (load (as-raw x) (raw 2))))))
   (else (as-raw x))))

(define (by-value? (raw kind) k)
  (declare unsafe inline)
  (and (raw= kind eqv-kind) (or (bignum? k) (ratnum? k))))

;; Symbols by name, so they never need placing again
(define (key-hash (raw kind) k)
  (declare unsafe noalloc (result raw))
  (cond
   ((symbol? k) (symbol-hash-word k))
   ((by-value? kind k) (number-hash k))
   (else (as-raw k))))

;; Whether the hash of k changes when a collection moves it
(define (address-key? (raw kind) k)
  (declare unsafe inline)
  (and (not (raw= kind generic-kind))
       (or (pair? k)
           (and (tag=? k (raw 0))
                (not (symbol? k))
                (not (by-value? kind k))))))

(define (stale? t)
  (declare unsafe inline)
  (and (not (raw= (load (as-raw t) (raw 2)) (raw 0)))
       (not (raw= (load (as-raw t) (raw 3)) (load gc-count (raw 0))))))

;; Normalized numbers, so bignums compare by words
(define (eqv-key=? x y)
  (declare unsafe noalloc)
  (or (eq? x y)
      (and (bignum? x) (bignum? y)
           (raw= (load (as-raw x) (raw 1)) (load (as-raw y) (raw 1)))
           (words=? (raw+ (as-raw x) (raw 16)) (raw+ (as-raw y) (raw 16))
                    (raw+ (load (as-raw x) (raw 1)) (raw 1))))
      (and (ratnum? x) (ratnum? y)
           (eqv-key=? (as-val (load (as-raw x) (raw 1))) (as-val (load (as-raw y) (raw 1))))
           (eqv-key=? (as-val (load (as-raw x) (raw 2))) (as-val (load (as-raw y) (raw 2)))))))

;; The fixnum slot of k in an eq or eqv table, else of the empty slot where it goes; -1 when
;; a collection moved the keys since they were placed
(define (eq-slot t k)
  (declare unsafe noalloc)
  (if (stale? t)
      -1
      (let ((s (table-storage t))
            (value? (by-value? (table-kind t) k)))
        (let probe ((i (home-slot s (key-hash (table-kind t) k))))
          (let ((x (slot-word s i (raw 0))))
            (if (or (raw= x (raw 0))
                    (raw= x (as-raw k))
                    (and value? (eqv-key=? (as-val x) k)))
                (make-fixnum i)
                (probe (next-slot s i))))))))

;; Stores k and v in the first empty slot from the home of k, in the body s of an eq or eqv table
(define (eq-place! (raw s) k v)
  (declare unsafe noalloc (result void))
  (let probe ((i (home-slot s (key-hash (load s (raw 5)) k))))
    (if (raw= (slot-word s i (raw 0)) (raw 0))
        (set-slot! s i k v)
        (probe (next-slot s i)))))

;; Places the entries of the eq or eqv table body old in the empty body new, at the current
;; addresses
(define (reinsert! old new)
  (declare unsafe noalloc (result void))
  (store (as-raw new) (raw 3) (load gc-count (raw 0)))
  (let entry-next ((i (raw 0)))
    (if (raw= i (storage-slots (as-raw old)))
        undef
        (begin
          (unless (raw= (slot-word (as-raw old) i (raw 0)) (raw 0))
            (eq-place! (as-raw new) (as-val (slot-word (as-raw old) i (raw 0)))
                       (as-val (slot-word (as-raw old) i (raw 1)))))
          (entry-next (raw+ i (raw 1)))))))

;; Empties slot i, then places again the entries after it, up to an empty slot
(define (eq-delete-slot! t (raw i))
  (declare unsafe noalloc (result void))
  (let ((s (table-storage t)))
    (clear-slot! s i)
    (let entry-next ((j (next-slot s i)))
      (let ((k (slot-word s j (raw 0)))
            (v (slot-word s j (raw 1))))
        (if (raw= k (raw 0))
            undef
            (begin
              (clear-slot! s j)
              (eq-place! s (as-val k) (as-val v))
              (entry-next (next-slot s j))))))))

;; The fixnum hash of k, by the hash procedure of t
(define (generic-hash t k)
  (declare unsafe)
  (let ((h ((table-hash t) k)))
    (cond
     ((fixnum? h) h)
     ((bignum? h) (hash-fixnum (load (as-raw h) (raw 3))))
     (else (hash-function-error (table-hash (resolve t)) h k)))))

;; A fixnum slot from the fixnum hash h
(define (generic-home t h)
  (declare unsafe inline)
  (make-fixnum (home-slot (table-storage (resolve t)) (fixnum-value h))))

(define (generic-next t i)
  (declare unsafe inline)
  (make-fixnum (next-slot (table-storage (resolve t)) (fixnum-value i))))

;; The calls may collect or relocate t, so only fixnum slots are kept
(define (generic-slot t k)
  (declare unsafe)
  (let probe ((i (generic-home t (generic-hash t k))))
    (let ((x (as-val (slot-word (table-storage (resolve t)) (fixnum-value i) (raw 0)))))
      (if (or (raw= (as-raw x) (raw 0))
              ((table-equiv (resolve t)) x k))
          i
          (probe (generic-next t i))))))

(define (generic-place! t k v)
  (declare unsafe)
  (let probe ((i (generic-home t (generic-hash t k))))
    (if (slot-empty? (resolve t) i)
        (begin
          (set-slot! (table-storage (resolve t)) (fixnum-value i) k v)
          undef)
        (probe (generic-next t i)))))

;; Places the entries of the generic table body old in the empty body new
(define (generic-reinsert! old new)
  (declare unsafe)
  (let entry-next ((i 0))
    (if (raw= (fixnum-value i) (storage-slots (as-raw old)))
        undef
        (let ((k (as-val (slot-word (as-raw old) (fixnum-value i) (raw 0)))))
          (unless (raw= (as-raw k) (raw 0))
            (generic-place! new k (as-val (slot-word (as-raw old) (fixnum-value i) (raw 1)))))
          (entry-next (+ i 1))))))

(define (generic-delete-slot! t i)
  (declare unsafe)
  (clear-slot! (table-storage (resolve t)) (fixnum-value i))
  (let entry-next ((j (generic-next t i)))
    (let ((k (as-val (slot-word (table-storage (resolve t)) (fixnum-value j) (raw 0))))
          (v (as-val (slot-word (table-storage (resolve t)) (fixnum-value j) (raw 1)))))
      (if (raw= (as-raw k) (raw 0))
          undef
          (begin
            (clear-slot! (table-storage (resolve t)) (fixnum-value j))
            (generic-place! t k v)
            (entry-next (generic-next t j)))))))

;; 64 - k for n = 2^k slots
(define (slots-shift (raw n))
  (declare unsafe noalloc (result raw))
  (let shift-next ((n n)
                   (shift (raw 64)))
    (if (raw= n (raw 1))
        shift
        (shift-next (raw>>u n (raw 1)) (raw- shift (raw 1))))))

;; An empty table body of the fixnum kind, with n slots, a fixnum power of 2
(define (make-table kind n hash equiv)
  (declare unsafe)
  (let ((p (alloc (raw+ (raw<< (fixnum-value n) (raw 1)) (raw 8)))))
    (store p (raw 0) hashtable-type)
    (store p (raw 3) (load gc-count (raw 0)))
    (store p (raw 4) (slots-shift (fixnum-value n)))
    (store p (raw 5) (fixnum-value kind))
    (store p (raw 6) hash)
    (store p (raw 7) equiv)
    (as-val p)))

;; The new body of t, with its entries placed in n slots, t relocated to it
(define (resize! t n)
  (declare unsafe)
  (let ((new (make-table (make-fixnum (table-kind t)) n (table-hash t) (table-equiv t))))
    (store (as-raw new) (raw 1) (table-count t))
    (store (as-raw new) (raw 2) (load (as-raw t) (raw 2)))
    (if (raw= (table-kind t) generic-kind)
        (generic-reinsert! t new)
        (begin
          (reinsert! t new)
          undef))
    (relocate! (resolve t) new)
    new))

;; Nothing allocates between the placing and the probe
(define (replaced-slot t k)
  (declare unsafe)
  (eq-slot (resize! t (make-fixnum (storage-slots (table-storage t)))) k))

;; The fixnum slot of k, else of the empty slot where it goes; the body of t may be replaced,
;; so the caller resolves t after
(define (table-slot t k)
  (declare unsafe inline)
  (if (raw= (table-kind t) generic-kind)
      (generic-slot t k)
      (let ((i (eq-slot t k)))
        (if (eq? i -1)
            (replaced-slot t k)
            i))))

;; The slots for k entries: a power of 2, at least 8 and 2k
(define (table-slots k)
  (let slots-next ((n 8))
    (if (< n (* 2 k))
        (slots-next (* 2 n))
        n)))

(define (table-size? k)
  (declare unsafe inline)
  (and (fixnum? k) (raw>= (fixnum-value k) (raw 0))))

(define (make-eq-hashtable)
  (declare public)
  (make-table 0 8 #f #f))

(define (variadic-make-eq-hashtable . k)
  (cond
   ((null? k) (make-eq-hashtable))
   ((and (null? (cdr k)) (table-size? (car k))) (make-table 0 (table-slots (car k)) #f #f))
   ((null? (cdr k)) (argument-error make-eq-hashtable 'size-argument (car k)))
   (else (argument-count-error make-eq-hashtable (length k)))))

(define (make-eqv-hashtable)
  (declare public)
  (make-table 1 8 #f #f))

(define (variadic-make-eqv-hashtable . k)
  (cond
   ((null? k) (make-eqv-hashtable))
   ((and (null? (cdr k)) (table-size? (car k))) (make-table 1 (table-slots (car k)) #f #f))
   ((null? (cdr k)) (argument-error make-eqv-hashtable 'size-argument (car k)))
   (else (argument-count-error make-eqv-hashtable (length k)))))

(define (check-hash-procedures hash equiv)
  (declare inline (result void))
  (unless (procedure? hash)
    (argument-error make-hashtable 'procedure hash))
  (unless (procedure? equiv)
    (argument-error make-hashtable 'procedure equiv)))

(define (make-hashtable hash equiv)
  (declare unsafe public)
  (check-hash-procedures hash equiv)
  (make-table 2 8 hash equiv))

(define (variadic-make-hashtable hash equiv . k)
  (check-hash-procedures hash equiv)
  (cond
   ((null? k) (make-table 2 8 hash equiv))
   ((and (null? (cdr k)) (table-size? (car k))) (make-table 2 (table-slots (car k)) hash equiv))
   ((null? (cdr k)) (argument-error make-hashtable 'size-argument (car k)))
   (else (argument-count-error make-hashtable (+ 2 (length k))))))

(define (hashtable-size t)
  (declare unsafe public)
  (make-fixnum (table-count (hashtable-body t hashtable-size))))

(define (hashtable-ref t k default)
  (declare unsafe public)
  (let* ((i (table-slot (hashtable-body t hashtable-ref) k))
         (b (resolve t)))
    (if (slot-empty? b i)
        default
        (as-val (slot-word (table-storage b) (fixnum-value i) (raw 1))))))

(define (hashtable-contains? t k)
  (declare unsafe public)
  (let ((i (table-slot (hashtable-body t hashtable-contains?) k)))
    (not (slot-empty? (resolve t) i))))

;; Grows first, so that the slot found stays valid
(define (hashtable-set! t k v)
  (declare unsafe public)
  (let ((b (hashtable-body t hashtable-set!)))
    (when (raw>u (raw<< (raw+ (table-count b) (raw 1)) (raw 1)) (storage-slots (table-storage b)))
      (resize! b (make-fixnum (raw<< (storage-slots (table-storage b)) (raw 1))))))
  (let* ((i (table-slot (resolve t) k))
         (b (resolve t)))
    (if (slot-empty? b i)
        (begin
          (set-slot! (table-storage b) (fixnum-value i) k v)
          (store (as-raw b) (raw 1) (raw+ (table-count b) (raw 1)))
          (when (address-key? (table-kind b) k)
            (when (raw= (load (as-raw b) (raw 2)) (raw 0))
              (store (as-raw b) (raw 3) (load gc-count (raw 0))))
            (store (as-raw b) (raw 2) (raw+ (load (as-raw b) (raw 2)) (raw 1))))
          undef)
        (begin
          (store (table-storage b) (raw+ (raw<< (fixnum-value i) (raw 1)) (raw 9)) v)
          undef))))

(define (hashtable-update! t k proc default)
  (declare public)
  (unless (hashtable? t)
    (argument-error hashtable-update! 'hashtable t))
  (hashtable-set! t k (proc (hashtable-ref t k default))))

(define (hashtable-delete! t k)
  (declare unsafe public)
  (let* ((i (table-slot (hashtable-body t hashtable-delete!) k))
         (b (resolve t)))
    (unless (slot-empty? b i)
      (store (as-raw b) (raw 1) (raw- (table-count b) (raw 1)))
      (when (address-key? (table-kind b) k)
        (store (as-raw b) (raw 2) (raw- (load (as-raw b) (raw 2)) (raw 1))))
      (if (raw= (table-kind b) generic-kind)
          (generic-delete-slot! b i)
          (begin
            (eq-delete-slot! b (fixnum-value i))
            undef)))
    undef))

(define (hashtable-clear! t)
  (declare unsafe public)
  (let* ((b (hashtable-body t hashtable-clear!))
         (new (make-table (make-fixnum (table-kind b)) 8 (table-hash b) (table-equiv b))))
    (relocate! (resolve t) new)
    undef))

;; A vector of the keys (j 0) or the values (j 1), in slot order
(define (table-column t (raw j))
  (declare unsafe inline)
  (let ((v (make-vector (make-fixnum (table-count t)) 0)))
    (fill-column! t v j)
    v))

(define (fill-column! t v (raw j))
  (declare unsafe noalloc (result void))
  (let entry-next ((i (raw 0))
                   (n (raw 0)))
    (cond
     ((raw= n (table-count t)) undef)
     ((raw= (slot-word (table-storage t) i (raw 0)) (raw 0)) (entry-next (raw+ i (raw 1)) n))
     (else
      (store (as-raw v) (raw+ n (raw 2)) (slot-word (table-storage t) i j))
      (entry-next (raw+ i (raw 1)) (raw+ n (raw 1)))))))

(define (hashtable-keys t)
  (declare unsafe public)
  (table-column (hashtable-body t hashtable-keys) (raw 0)))

(define (hashtable-values t)
  (declare unsafe public)
  (table-column (hashtable-body t hashtable-values) (raw 1)))

(define (hashtable-entries t)
  (declare unsafe public)
  (let* ((b (hashtable-body t hashtable-entries))
         (keys (table-column b (raw 0)))
         (vals (table-column b (raw 1))))
    (values keys vals)))

;; Agrees with equal?, never by address: other objects by their header
(define (equal-hash-word x)
  (declare unsafe noalloc (result raw))
  (cond
   ((pair? x)
    (let pair-next ((x x)
                    (h (raw 3)))
      (if (pair? x)
          (pair-next (pair-cdr x) (mix h (equal-hash-word (pair-car x))))
          (mix h (equal-hash-word x)))))
   ((not (tag=? x (raw 0))) (as-raw x))
   ((raw= (as-raw x) (raw 0)) (raw 0))
   ((symbol? x) (symbol-hash-word x))
   ((string? x) (string-hash-word (resolve x)))
   ((vector? x)
    (let element-next ((i (raw 0))
                       (h (load (as-raw x) (raw 1))))
      (if (raw= i (load (as-raw x) (raw 1)))
          h
          (element-next (raw+ i (raw 1))
                        (mix h (equal-hash-word (as-val (load (as-raw x) (raw+ i (raw 2))))))))))
   ((bytevector? x)
    (let word-next ((i (raw 2))
                    (h (bytevector-nbytes x)))
      (if (raw= i (bytevector-words (bytevector-nbytes x)))
          h
          (word-next (raw+ i (raw 1)) (mix h (load (as-raw x) i))))))
   ((or (bignum? x) (ratnum? x)) (number-hash x))
   (else (load (as-raw (resolve x)) (raw 0)))))

(define (equal-hash x)
  (declare unsafe public)
  (hash-fixnum (equal-hash-word x)))

(define (string-hash s)
  (declare unsafe public)
  (hash-fixnum (string-hash-word (string-body s string-hash))))

(define (symbol-hash s)
  (declare unsafe public)
  (unless (symbol? s)
    (argument-error symbol-hash 'symbol s))
  (hash-fixnum (symbol-hash-word s)))

(define (display-hashtable (raw out) t)
  (declare unsafe noalloc (result void))
  (cond
   ((raw= (table-kind t) eq-kind) (put-string out "#<eq hashtable>"))
   ((raw= (table-kind t) eqv-kind) (put-string out "#<eqv hashtable>"))
   (else (put-string out "#<hashtable>"))))
