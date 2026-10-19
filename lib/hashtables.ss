;; Hashtables (R6RS): [hashtable-type, count, naddr, epoch, shift, kind, storage, hash, equiv].
;; count, naddr (the keys hashed by address), epoch (gc-count when they were placed), shift
;; (64 - k for 2^k slots) and kind are raw words; storage is a vector of 2^k key and value
;; pairs, a key 0 when empty, at most half full; hash and equiv are the procedures of a generic
;; table, else #f.
;; Linear probing from the top k bits of the mixed hash, as for symbols.
;; A collection moves the keys hashed by address, so the next lookup places them again.

(define-constant eq-kind (raw 0))
(define-constant eqv-kind (raw 1))
(define-constant generic-kind (raw 2))
(define-constant golden (raw 11400714819323198485)) ; 0x9E3779B97F4A7C15

(define (hashtable? x)
  (declare unsafe inline public)
  (and (tag=? x (raw 0))
       (not (raw= (as-raw x) (raw 0)))
       (raw= (load (as-raw x) (raw 0)) hashtable-type)))

(define (check-hashtable t (raw msg))
  (declare unsafe inline (result void))
  (unless (hashtable? t)
    (scheme-error msg t)))

(define (table-count t)
  (declare unsafe inline (result raw))
  (load (as-raw t) (raw 1)))

(define (table-kind t)
  (declare unsafe inline (result raw))
  (load (as-raw t) (raw 5)))

(define (table-storage t)
  (declare unsafe inline (result raw))
  (load (as-raw t) (raw 6)))

(define (storage-slots (raw s))
  (declare unsafe inline (result raw))
  (raw>>u (load s (raw 1)) (raw 1)))

;; Word j of slot i of the storage s: 0 the key, 1 the value
(define (slot-word (raw s) (raw i) (raw j))
  (declare unsafe inline (result raw))
  (load s (raw+ (raw<< i (raw 1)) (raw+ j (raw 2)))))

(define (set-slot! (raw s) (raw i) k v)
  (declare unsafe inline (result void))
  (store s (raw+ (raw<< i (raw 1)) (raw 2)) k)
  (store s (raw+ (raw<< i (raw 1)) (raw 3)) v))

(define (clear-slot! (raw s) (raw i))
  (declare unsafe inline (result void))
  (store s (raw+ (raw<< i (raw 1)) (raw 2)) (raw 0))
  (store s (raw+ (raw<< i (raw 1)) (raw 3)) (raw 0)))

;; The fixnum slot i of t
(define (slot-empty? t i)
  (declare unsafe inline)
  (raw= (slot-word (table-storage t) (fixnum-value i) (raw 0)) (raw 0)))

(define (next-slot (raw s) (raw i))
  (declare unsafe inline (result raw))
  (bit-and (raw+ i (raw 1)) (raw- (storage-slots s) (raw 1))))

(define (home-slot t (raw h))
  (declare unsafe inline (result raw))
  (raw>>u (raw* h golden) (load (as-raw t) (raw 4))))

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
       (or (tag=? k (raw 3))
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
        (let probe ((i (home-slot t (key-hash (table-kind t) k))))
          (let ((x (slot-word s i (raw 0))))
            (if (or (raw= x (raw 0))
                    (raw= x (as-raw k))
                    (and value? (eqv-key=? (as-val x) k)))
                (make-fixnum i)
                (probe (next-slot s i))))))))

;; Stores k and v in the first empty slot from the home of k, in the storage s of t
(define (eq-place! t (raw s) k v)
  (declare unsafe noalloc (result void))
  (let probe ((i (home-slot t (key-hash (table-kind t) k))))
    (if (raw= (slot-word s i (raw 0)) (raw 0))
        (set-slot! s i k v)
        (probe (next-slot s i)))))

;; Makes s the storage of t, placed at the current addresses
(define (set-storage! t s)
  (declare unsafe noalloc (result void))
  (store (as-raw t) (raw 6) s)
  (store (as-raw t) (raw 3) (load gc-count (raw 0)))
  (let shift-next ((n (storage-slots (as-raw s)))
                   (shift (raw 64)))
    (if (raw= n (raw 1))
        (store (as-raw t) (raw 4) shift)
        (shift-next (raw>>u n (raw 1)) (raw- shift (raw 1))))))

;; Moves the entries of an eq or eqv table to the new storage s
(define (reinsert! t s)
  (declare unsafe noalloc (result void))
  (let ((old (table-storage t)))
    (set-storage! t s)
    (let entry-next ((i (raw 0)))
      (if (raw= i (storage-slots old))
          undef
          (begin
            (unless (raw= (slot-word old i (raw 0)) (raw 0))
              (eq-place! t (as-raw s) (as-val (slot-word old i (raw 0)))
                         (as-val (slot-word old i (raw 1)))))
            (entry-next (raw+ i (raw 1))))))))

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
              (eq-place! t s (as-val k) (as-val v))
              (entry-next (next-slot s j))))))))

;; The fixnum hash of k, by the hash procedure of t
(define (generic-hash t k)
  (declare unsafe)
  (let ((h ((as-val (load (as-raw t) (raw 7))) k)))
    (cond
     ((fixnum? h) h)
     ((bignum? h) (hash-fixnum (load (as-raw h) (raw 3))))
     (else (scheme-error "hashtable hash function returned a non-integer" h)))))

;; A fixnum slot from the fixnum hash h
(define (generic-home t h)
  (declare unsafe inline)
  (make-fixnum (home-slot t (fixnum-value h))))

(define (generic-next t i)
  (declare unsafe inline)
  (make-fixnum (next-slot (table-storage t) (fixnum-value i))))

;; The calls may collect, so only fixnum slots are kept
(define (generic-slot t k)
  (declare unsafe)
  (let probe ((i (generic-home t (generic-hash t k))))
    (let ((x (as-val (slot-word (table-storage t) (fixnum-value i) (raw 0)))))
      (if (or (raw= (as-raw x) (raw 0))
              ((as-val (load (as-raw t) (raw 8))) x k))
          i
          (probe (generic-next t i))))))

(define (generic-place! t k v)
  (declare unsafe)
  (let probe ((i (generic-home t (generic-hash t k))))
    (if (slot-empty? t i)
        (begin
          (set-slot! (table-storage t) (fixnum-value i) k v)
          undef)
        (probe (generic-next t i)))))

(define (generic-reinsert! t s)
  (declare unsafe)
  (let ((old (as-val (table-storage t))))
    (set-storage! t s)
    (let entry-next ((i 0))
      (if (raw= (fixnum-value i) (storage-slots (as-raw old)))
          undef
          (let ((k (as-val (slot-word (as-raw old) (fixnum-value i) (raw 0)))))
            (unless (raw= (as-raw k) (raw 0))
              (generic-place! t k (as-val (slot-word (as-raw old) (fixnum-value i) (raw 1)))))
            (entry-next (+ i 1)))))))

(define (generic-delete-slot! t i)
  (declare unsafe)
  (clear-slot! (table-storage t) (fixnum-value i))
  (let entry-next ((j (generic-next t i)))
    (let ((k (as-val (slot-word (table-storage t) (fixnum-value j) (raw 0))))
          (v (as-val (slot-word (table-storage t) (fixnum-value j) (raw 1)))))
      (if (raw= (as-raw k) (raw 0))
          undef
          (begin
            (clear-slot! (table-storage t) (fixnum-value j))
            (generic-place! t k v)
            (entry-next (generic-next t j)))))))

;; A zeroed storage of n slots, a fixnum
(define (make-storage n)
  (declare unsafe inline)
  (let ((p (alloc (raw+ (raw<< (fixnum-value n) (raw 1)) (raw 2)))))
    (store p (raw 0) vector-type)
    (store p (raw 1) (raw<< (fixnum-value n) (raw 1)))
    (as-val p)))

;; Places the entries in a new storage of n slots
(define (resize! t n)
  (declare unsafe)
  (let ((s (make-storage n)))
    (if (raw= (table-kind t) generic-kind)
        (generic-reinsert! t s)
        (begin
          (reinsert! t s)
          undef))))

;; Nothing allocates between the placing and the probe
(define (replaced-slot t k)
  (declare unsafe)
  (resize! t (make-fixnum (storage-slots (table-storage t))))
  (eq-slot t k))

;; The fixnum slot of k, else of the empty slot where it goes
(define (table-slot t k)
  (declare unsafe inline)
  (if (raw= (table-kind t) generic-kind)
      (generic-slot t k)
      (let ((i (eq-slot t k)))
        (if (eq? i -1)
            (replaced-slot t k)
            i))))

(define (make-table kind n hash equiv)
  (declare unsafe)
  (let* ((s (make-storage n))
         (p (alloc (raw 9))))
    (store p (raw 0) hashtable-type)
    (store p (raw 5) (fixnum-value kind))
    (store p (raw 7) hash)
    (store p (raw 8) equiv)
    (set-storage! (as-val p) s)
    (as-val p)))

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
  (declare unsafe)
  (unless (and (pair? k) (null? (cdr k)) (table-size? (car k)))
    (scheme-error "`make-eq-hashtable` expected an optional nonnegative fixnum" k))
  (make-table 0 (table-slots (car k)) #f #f))

(define (make-eqv-hashtable)
  (declare public)
  (make-table 1 8 #f #f))

(define (variadic-make-eqv-hashtable . k)
  (declare unsafe)
  (unless (and (pair? k) (null? (cdr k)) (table-size? (car k)))
    (scheme-error "`make-eqv-hashtable` expected an optional nonnegative fixnum" k))
  (make-table 1 (table-slots (car k)) #f #f))

(define (check-hash-procedures hash equiv)
  (declare unsafe inline (result void))
  (unless (procedure? hash)
    (scheme-error "`make-hashtable` expected a procedure" hash))
  (unless (procedure? equiv)
    (scheme-error "`make-hashtable` expected a procedure" equiv)))

(define (make-hashtable hash equiv)
  (declare unsafe public)
  (check-hash-procedures hash equiv)
  (make-table 2 8 hash equiv))

(define (variadic-make-hashtable hash equiv . k)
  (declare unsafe)
  (check-hash-procedures hash equiv)
  (unless (and (pair? k) (null? (cdr k)) (table-size? (car k)))
    (scheme-error "`make-hashtable` expected an optional nonnegative fixnum" k))
  (make-table 2 (table-slots (car k)) hash equiv))

(define (hashtable-size t)
  (declare unsafe public)
  (check-hashtable t "`hashtable-size` expected a hashtable")
  (make-fixnum (table-count t)))

(define (hashtable-ref t k default)
  (declare unsafe public)
  (check-hashtable t "`hashtable-ref` expected a hashtable")
  (let ((i (table-slot t k)))
    (if (slot-empty? t i)
        default
        (as-val (slot-word (table-storage t) (fixnum-value i) (raw 1))))))

(define (hashtable-contains? t k)
  (declare unsafe public)
  (check-hashtable t "`hashtable-contains?` expected a hashtable")
  (not (slot-empty? t (table-slot t k))))

;; Grows first, so that the slot found stays valid
(define (hashtable-set! t k v)
  (declare unsafe public)
  (check-hashtable t "`hashtable-set!` expected a hashtable")
  (when (raw>u (raw<< (raw+ (table-count t) (raw 1)) (raw 1)) (storage-slots (table-storage t)))
    (resize! t (make-fixnum (raw<< (storage-slots (table-storage t)) (raw 1)))))
  (let ((i (table-slot t k)))
    (if (slot-empty? t i)
        (begin
          (set-slot! (table-storage t) (fixnum-value i) k v)
          (store (as-raw t) (raw 1) (raw+ (table-count t) (raw 1)))
          (when (address-key? (table-kind t) k)
            (when (raw= (load (as-raw t) (raw 2)) (raw 0))
              (store (as-raw t) (raw 3) (load gc-count (raw 0))))
            (store (as-raw t) (raw 2) (raw+ (load (as-raw t) (raw 2)) (raw 1))))
          undef)
        (begin
          (store (table-storage t) (raw+ (raw<< (fixnum-value i) (raw 1)) (raw 3)) v)
          undef))))

(define (hashtable-update! t k proc default)
  (declare unsafe public)
  (check-hashtable t "`hashtable-update!` expected a hashtable")
  (hashtable-set! t k (proc (hashtable-ref t k default))))

(define (hashtable-delete! t k)
  (declare unsafe public)
  (check-hashtable t "`hashtable-delete!` expected a hashtable")
  (let ((i (table-slot t k)))
    (unless (slot-empty? t i)
      (store (as-raw t) (raw 1) (raw- (table-count t) (raw 1)))
      (when (address-key? (table-kind t) k)
        (store (as-raw t) (raw 2) (raw- (load (as-raw t) (raw 2)) (raw 1))))
      (if (raw= (table-kind t) generic-kind)
          (generic-delete-slot! t i)
          (begin
            (eq-delete-slot! t (fixnum-value i))
            undef)))
    undef))

(define (hashtable-clear! t)
  (declare unsafe public)
  (check-hashtable t "`hashtable-clear!` expected a hashtable")
  (let ((s (make-storage 8)))
    (store (as-raw t) (raw 1) (raw 0))
    (store (as-raw t) (raw 2) (raw 0))
    (set-storage! t s)
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
  (check-hashtable t "`hashtable-keys` expected a hashtable")
  (table-column t (raw 0)))

(define (hashtable-values t)
  (declare unsafe public)
  (check-hashtable t "`hashtable-values` expected a hashtable")
  (table-column t (raw 1)))

(define (hashtable-entries t)
  (declare unsafe public)
  (check-hashtable t "`hashtable-entries` expected a hashtable")
  (let* ((keys (table-column t (raw 0)))
         (vals (table-column t (raw 1))))
    (values keys vals)))

;; Agrees with equal?, never by address: other objects by their header
(define (equal-hash-word x)
  (declare unsafe noalloc (result raw))
  (cond
   ((pair? x)
    (let pair-next ((x x)
                    (h (raw 3)))
      (if (pair? x)
          (pair-next (cdr x) (mix h (equal-hash-word (car x))))
          (mix h (equal-hash-word x)))))
   ((not (tag=? x (raw 0))) (as-raw x))
   ((raw= (as-raw x) (raw 0)) (raw 0))
   ((symbol? x) (symbol-hash-word x))
   ((string? x) (string-hash-word x))
   ((vector? x)
    (let element-next ((i (raw 0))
                       (h (load (as-raw x) (raw 1))))
      (if (raw= i (load (as-raw x) (raw 1)))
          h
          (element-next (raw+ i (raw 1))
                        (mix h (equal-hash-word (as-val (load (as-raw x) (raw+ i (raw 2))))))))))
   ((or (bignum? x) (ratnum? x)) (number-hash x))
   (else (load (as-raw x) (raw 0)))))

(define (equal-hash x)
  (declare unsafe public)
  (hash-fixnum (equal-hash-word x)))

(define (string-hash s)
  (declare unsafe public)
  (check-string s "`string-hash` expected a string")
  (hash-fixnum (string-hash-word s)))

(define (symbol-hash s)
  (declare unsafe public)
  (unless (symbol? s)
    (scheme-error "`symbol-hash` expected a symbol" s))
  (hash-fixnum (symbol-hash-word s)))

(define (display-hashtable (raw out) t)
  (declare unsafe noalloc (result void))
  (cond
   ((raw= (table-kind t) eq-kind) (put-string out "#<eq hashtable>"))
   ((raw= (table-kind t) eqv-kind) (put-string out "#<eqv hashtable>"))
   (else (put-string out "#<hashtable>"))))
