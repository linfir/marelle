;; Symbols: [symbol-type, hash, name], the name an immutable string, the hash a raw word, of
;; the name's code points; interned in symbol-table: open addressing over a vector of 2^k slots,
;; #f when empty, linear probing, keyed on the hash; doubled when half full. The table is strong:
;; symbols are never collected.
;; Gensyms: [gensym-type, hash, name], uninterned, the hash their number, so unique; printed by
;; their name, as Chez does when print-gensym is #f

;; The table, a vector, or 0 before the first symbol; collect forwards it
(define-data symbol-table 1)
;; Its number of symbols, and 64 - k: a hash's slot is its top k bits
(define-data symbol-count 1)
(define-data symbol-shift 1)
;; The number of gensyms made
(define-data gensym-count 1)

(define (symbol-header? (raw h))
  (declare unsafe inline)
  (raw= (bit-and h (raw -9)) symbol-type))

(define (symbol? x)
  (declare unsafe inline public)
  (and (tag=? x (raw 0))
       (not (raw= (as-raw x) (raw 0)))
       (symbol-header? (load (as-raw x) (raw 0)))))

(define (gensym? x)
  (declare unsafe inline public)
  (and (tag=? x (raw 0))
       (not (raw= (as-raw x) (raw 0)))
       (raw= (load (as-raw x) (raw 0)) gensym-type)))

(define (symbol-name sym)
  (declare unsafe inline)
  (as-val (load (as-raw sym) (raw 2))))

(define (symbol-hash-word sym)
  (declare unsafe inline (result raw))
  (load (as-raw sym) (raw 1)))

(define (symbol->string sym)
  (declare unsafe inline public)
  (unless (symbol? sym)
    (argument-error symbol->string 'symbol sym))
  (symbol-name sym))

(define (make-symbol s)
  (declare unsafe inline)
  (let ((p (alloc (raw 3))))
    (store p (raw 0) symbol-type)
    (store p (raw 1) (string-hash-word s))
    (store p (raw 2) s)
    (as-val p)))

(define (make-gensym s)
  (declare unsafe inline)
  (let ((p (alloc (raw 3)))
        (n (load gensym-count (raw 0))))
    (store gensym-count (raw 0) (raw+ n (raw 1)))
    (store p (raw 0) gensym-type)
    (store p (raw 1) n)
    (store p (raw 2) s)
    (as-val p)))

(define (gensym name)
  (declare public)
  (check-string name gensym)
  (make-gensym name))

(define (numbered-name n)
  (string-append "g" (number->string n)))

;; With no argument, named g and its number
(define (variadic-gensym . name)
  (declare unsafe)
  (cond
   ((null? name) (make-gensym (numbered-name (make-fixnum (load gensym-count (raw 0))))))
   ((null? (cdr name)) (gensym (car name)))
   (else (argument-count-error gensym (length name)))))

;; The hash of the code points of the string s
(define (string-hash-word s)
  (declare unsafe noalloc (result raw))
  (let hash-next ((i (raw 0))
                  (h (raw 0)))
    (if (raw= i (string-end s))
        h
        (hash-next (string-next s i)
                   (raw* (raw+ h (raw+ (string-code-at s i) (raw 1)))
                         (raw 11400714819323198485)))))) ; 0x9E3779B97F4A7C15

;; The fixnum index of the slot of table holding the symbol named s, of hash h, else of the
;; empty slot where it goes
(define (symbol-slot table s (raw h))
  (declare unsafe noalloc)
  (let ((mask (raw- (load (as-raw table) (raw 1)) (raw 1))))
    (let probe ((i (raw>>u h (load symbol-shift (raw 0)))))
      (let ((x (as-val (load (as-raw table) (raw+ i (raw 2))))))
        (if (or (eq? x #f)
                (and (raw= (symbol-hash-word x) h) (same-string? (symbol-name x) s)))
            (make-fixnum i)
            (probe (bit-and (raw+ i (raw 1)) mask)))))))

;; Stores the symbols of the table old in the table new, from their hashes
(define (rehash-symbols! old new)
  (declare unsafe noalloc (result void))
  (let symbol-next ((i (raw 0)))
    (if (raw= i (load (as-raw old) (raw 1)))
        undef
        (let ((x (as-val (load (as-raw old) (raw+ i (raw 2))))))
          (unless (eq? x #f)
            (store (as-raw new)
                   (raw+ (fixnum-value (symbol-slot new (symbol-name x) (symbol-hash-word x))) (raw 2))
                   x))
          (symbol-next (raw+ i (raw 1)))))))

(define (grow-symbol-table!)
  (declare unsafe)
  (let ((new (make-vector (* 2 (vector-length (as-val (load symbol-table (raw 0))))) #f)))
    (store symbol-shift (raw 0) (raw- (load symbol-shift (raw 0)) (raw 1)))
    (rehash-symbols! (as-val (load symbol-table (raw 0))) new)
    (store symbol-table (raw 0) new)
    undef))

;; The table, made the first time
(define (current-symbol-table)
  (declare unsafe inline)
  (when (raw= (load symbol-table (raw 0)) (raw 0))
    (store symbol-shift (raw 0) (raw 58))
    (store symbol-table (raw 0) (make-vector 64 #f)))
  (as-val (load symbol-table (raw 0))))

;; sym stored in the empty slot i of table, which grows when then more than half full
(define (insert-symbol! table i sym)
  (declare unsafe inline)
  (vector-set! table i sym)
  (store symbol-count (raw 0) (raw+ (load symbol-count (raw 0)) (raw 1)))
  (when (raw>u (raw<< (load symbol-count (raw 0)) (raw 1)) (load (as-raw table) (raw 1)))
    (grow-symbol-table!))
  sym)

;; The symbol named s, made the first time; s is shared, since strings are immutable
(define (string->symbol s)
  (declare unsafe public)
  (check-string s string->symbol)
  (let* ((table (current-symbol-table))
         (i (symbol-slot table s (string-hash-word s))))
    (or (vector-ref table i)
        (insert-symbol! table i (make-symbol s)))))

;; The program's static symbols (scheme_program's word 7: their count, then their addresses),
;; with their hashes, entered in the table before any other symbol is made, so string->symbol
;; finds them; the program's first startup call when it has any
(define (intern-static-symbols!)
  (declare unsafe)
  (let symbol-next ((i 0))
    (if (raw= (fixnum-value i) (load (load (address "scheme_program") (raw 7)) (raw 0)))
        undef
        (let* ((sym (as-val (load (load (address "scheme_program") (raw 7))
                                  (raw+ (fixnum-value i) (raw 1)))))
               (table (current-symbol-table))
               (k (symbol-slot table (symbol-name sym) (symbol-hash-word sym))))
          (when (vector-ref table k)
            (fatal "Static symbol interned twice\n"))
          (insert-symbol! table k sym)
          (symbol-next (+ i 1))))))
