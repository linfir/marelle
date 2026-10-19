;; Symbols: [symbol-type, name], the name an immutable string, interned in symbol-table: open
;; addressing over a vector of 2^k slots, #f when empty, linear probing, keyed on the hash of
;; the name's code points, cached in the string; doubled when half full. The table is strong:
;; symbols are never collected

;; The table, a vector, or 0 before the first symbol; collect forwards it
(define-data symbol-table 1)
;; Its number of symbols, and 64 - k: a hash's slot is its top k bits
(define-data symbol-count 1)
(define-data symbol-shift 1)

(define (symbol? x)
  (declare unsafe inline public)
  (and (tag=? x (raw 0))
       (not (raw= (as-raw x) (raw 0)))
       (raw= (load (as-raw x) (raw 0)) symbol-type)))

(define (symbol-name sym)
  (declare unsafe inline)
  (as-val (load (as-raw sym) (raw 1))))

(define (symbol->string sym)
  (declare unsafe inline public)
  (unless (symbol? sym)
    (scheme-error "`symbol->string` expected a symbol" sym))
  (symbol-name sym))

(define (make-symbol s)
  (declare unsafe inline)
  (let ((p (alloc (raw 2))))
    (store p (raw 0) symbol-type)
    (store p (raw 1) s)
    (as-val p)))

;; The hash of the code points of the string s, computed once and cached in it; never 0
(define (string-hash s)
  (declare unsafe noalloc (result raw))
  (if (raw= (load (as-raw s) (raw 2)) (raw 0))
      (let hash-next ((i (raw 0))
                      (h (raw 0)))
        (if (raw= i (string-nchars s))
            (let ((h (if (raw= h (raw 0)) (raw 1) h)))
              (store (as-raw s) (raw 2) h)
              h)
            (hash-next (raw+ i (raw 1))
                       (raw* (raw+ h (raw+ (string-code s i) (raw 1)))
                             (raw 11400714819323198485))))) ; 0x9E3779B97F4A7C15
      (load (as-raw s) (raw 2))))

;; The fixnum index of the slot of table holding the symbol named s, else of the empty slot
;; where it goes
(define (symbol-slot table s)
  (declare unsafe noalloc)
  (let ((h (string-hash s))
        (mask (raw- (load (as-raw table) (raw 1)) (raw 1))))
    (let probe ((i (raw>>u h (load symbol-shift (raw 0)))))
      (let ((x (as-val (load (as-raw table) (raw+ i (raw 2))))))
        (if (or (eq? x #f)
                (and (raw= (string-hash (symbol-name x)) h) (same-string? (symbol-name x) s)))
            (make-fixnum i)
            (probe (bit-and (raw+ i (raw 1)) mask)))))))

;; Stores the symbols of the table old in the table new, from their cached hashes
(define (rehash-symbols! old new)
  (declare unsafe noalloc (result void))
  (let symbol-next ((i (raw 0)))
    (if (raw= i (load (as-raw old) (raw 1)))
        undef
        (let ((x (as-val (load (as-raw old) (raw+ i (raw 2))))))
          (unless (eq? x #f)
            (store (as-raw new) (raw+ (fixnum-value (symbol-slot new (symbol-name x))) (raw 2)) x))
          (symbol-next (raw+ i (raw 1)))))))

(define (grow-symbol-table!)
  (declare unsafe)
  (let ((new (make-vector (* 2 (vector-length (as-val (load symbol-table (raw 0))))) #f)))
    (store symbol-shift (raw 0) (raw- (load symbol-shift (raw 0)) (raw 1)))
    (rehash-symbols! (as-val (load symbol-table (raw 0))) new)
    (store symbol-table (raw 0) new)
    undef))

;; The symbol named s, made the first time; s is shared, since strings are immutable
(define (string->symbol s)
  (declare unsafe public)
  (check-string s "`string->symbol` expected a string")
  (when (raw= (load symbol-table (raw 0)) (raw 0))
    (store symbol-shift (raw 0) (raw 58))
    (store symbol-table (raw 0) (make-vector 64 #f)))
  (let* ((table (as-val (load symbol-table (raw 0))))
         (i (symbol-slot table s)))
    (or (vector-ref table i)
        (let ((sym (make-symbol s)))
          (vector-set! table i sym)
          (store symbol-count (raw 0) (raw+ (load symbol-count (raw 0)) (raw 1)))
          (when (raw>u (raw<< (load symbol-count (raw 0)) (raw 1)) (load (as-raw table) (raw 1)))
            (grow-symbol-table!))
          sym))))
