;; GC state; the stubs in asm.sls use scheme-mem, scheme-len and scheme-limit
(define-data scheme-mem 1)
(define-data scheme-len 1)
(define-data scheme-cap 1)
(define-data scheme-limit 1)
(define-data spare-mem 1)
;; The size of the next collection's to-space
(define-data gc-target 1)
;; The words of a semispace at first, and at most: 8 MiB, in cache, to 512 MiB
(define-constant initial-heap-words (raw 1048576))
(define-constant max-heap-words (raw 67108864))
;; Statistics, printed at exit when SCHEME_GC_STATS is set
(define-data gc-count 1)
(define-data gc-copied 1)
(define-data gc-max-live 1)
(define-data gc-used-before 1)
;; Stress mode (SCHEME_GC_STRESS): collect every gc-stress allocations
(define-data gc-stress 1)
(define-data gc-countdown 1)
;; The to-space allocation pointer during a collection
(define-data gc-free 1)
;; Nonzero when SCHEME_GC_STATS is set and nonempty
(define-data gc-stats 1)

;; Each stretch between collections allocates from the words copied up to the used words
(define (print-gc-stats)
  (declare unsafe noalloc (result void))
  (put-string stderr "GC: ")
  (put-int stderr (load gc-count (raw 0)))
  (put-string stderr " collections, ")
  (put-int stderr (raw- (raw+ (load gc-used-before (raw 0)) (load scheme-len (raw 0)))
                        (load gc-copied (raw 0))))
  (put-string stderr " words allocated, ")
  (put-int stderr (load gc-copied (raw 0)))
  (put-string stderr " words copied, ")
  (put-int stderr (load gc-max-live (raw 0)))
  (put-string stderr " max live words\n"))

;; Counts down to the next forced collection in stress mode (SCHEME_GC_STRESS)
(define (gc-stress-due?)
  (declare unsafe inline)
  (and (not (raw= (load gc-stress (raw 0)) (raw 0)))
       (begin
         (store gc-countdown (raw 0) (raw- (load gc-countdown (raw 0)) (raw 1)))
         (raw= (load gc-countdown (raw 0)) (raw 0)))
       (begin
         (store gc-countdown (raw 0) (load gc-stress (raw 0)))
         #t)))

;; Copying collector (Cheney) with two semispaces:
;; scheme-mem is the current one, scheme-len its used words out of scheme-cap, the size of both
;; outside a collection; each space is zero past its used part, so frame slots not yet written
;; hold 0. The spaces start small (in cache) and double, up to max-heap-words: a collection leaving
;; more than a quarter of a space live sets gc-target to twice its size, and the next collection
;; copies into a to-space of that size, the old spaces unmapped.

(define (object-words (raw p))
  (declare unsafe noalloc (result raw))
  (cond
   ((raw= (load p (raw 0)) frame-type) (load p (raw 1)))
   ((raw= (load p (raw 0)) procedure-type) (raw+ (load p (raw 4)) (raw 5)))
   ((raw= (load p (raw 0)) bignum-type) (raw+ (load p (raw 1)) (raw 3)))
   ((raw= (load p (raw 0)) vector-type) (raw+ (load p (raw 1)) (raw 2)))
   ((raw= (load p (raw 0)) flexvector-type) (raw+ (load p (raw 2)) (raw 3)))
   ((raw= (load p (raw 0)) ratnum-type) (raw 3))
   ((raw= (load p (raw 0)) record-type) (raw+ (load p (raw 1)) (raw 3)))
   ((raw= (load p (raw 0)) hashtable-type) (raw 9))
   ((raw= (load p (raw 0)) port-type) (raw 6))
   ((raw= (load p (raw 0)) bytevector-type) (bytevector-words (load p (raw 1))))
   ((symbol-header? (load p (raw 0))) (raw 3))
   ((string-header? (load p (raw 0))) (string-words (load p (raw 0)) (load p (raw 1))))
   (else (raw 2)))) ; pair, box, values, record type or relocated object

;; The new address of the object x points to, copied to gc-free if it is in the
;; current space; a moved object holds forward-type and its new address
(define (forward (raw x))
  (declare unsafe noalloc (result raw))
  (let ((t (bit-and x (raw 7))))
    (if (not (or (raw= t (raw 0)) (raw= t (raw 3))))
        x
        (let ((p (bit-and x (raw -8)))
              (mem (load scheme-mem (raw 0))))
          (cond
           ((or (raw<u p mem)
                (raw>=u p (raw+ mem (raw<< (load scheme-len (raw 0)) (raw 3)))))
            x) ; null or static procedure
           ((raw= (load p (raw 0)) forward-type)
            (bit-or (load p (raw 1)) t))
           ;; A relocated object becomes its body's forwarder, so pointers to it reach the body
           ((raw= (load p (raw 0)) moved-type)
            (let ((new (forward (load p (raw 1)))))
              (store p (raw 0) forward-type)
              (store p (raw 1) new)
              new))
           (else
            (let ((new (load gc-free (raw 0)))
                  (n (if (raw= t (raw 3)) (raw 2) (object-words p))))
              (if (raw= t (raw 3))
                  (begin
                    (store new (raw 0) (load p (raw 0)))
                    (store new (raw 1) (load p (raw 1))))
                  (copy-words new p n))
              (store gc-free (raw 0) (raw+ new (raw<< n (raw 3))))
              (store p (raw 0) forward-type)
              (store p (raw 1) new)
              (bit-or new t))))))))

(define (forward-words (raw p) (raw n))
  (declare unsafe noalloc (result void))
  (let forward-next ((i (raw 0)))
    (if (raw= i n)
        undef
        (begin
          (store p i (forward (load p i)))
          (forward-next (raw+ i (raw 1)))))))

;; Forwards the values of the object at p in the new space, returns its length;
;; the raw slots of a frame are not scanned
(define (scan (raw p))
  (declare unsafe noalloc (result raw))
  (let* ((n (object-words p))
         (header (load p (raw 0)))
         (frame? (raw= header frame-type))
         (start (cond
                 (frame? (raw 4))
                 ((raw= header procedure-type) (raw 5))
                 ((raw= header box-type) (raw 1))
                 ((raw= header values-type) (raw 1))
                 ((symbol-header? header) (raw 2)) ; the name
                 ((raw= header bignum-type) n) ; only raw words
                 ((raw= header vector-type) (raw 2))
                 ((raw= header flexvector-type) (raw 3)) ; the elements
                 ((raw= header ratnum-type) (raw 1))
                 ((raw= header rtd-type) (raw 1)) ; the name
                 ((raw= header record-type) (raw 2)) ; the type and the fields
                 ((raw= header hashtable-type) (raw 6)) ; the storage and procedures
                 ((raw= header port-type) (raw 5)) ; the buffer
                 ((raw= header bytevector-type) n) ; only raw words
                 ((string-header? header) n) ; only raw words
                 (else (raw 0)))))
    (forward-words (raw+ p (raw<< start (raw 3)))
                   (raw- (if frame? (raw- n (load p (raw 2))) n) start))
    n))

;; Moves the live objects and updates the roots, which must hold every heap pointer in use;
;; the rest is reached through them
(define (collect (raw roots) (raw nroots))
  (declare unsafe noalloc (result void))
  (let* ((cap (load scheme-cap (raw 0)))
         (target (load gc-target (raw 0)))
         (grow? (raw>u target cap))
         (to (if grow? (map-words target) (load spare-mem (raw 0)))))
    (store gc-free (raw 0) to)
    (forward-words roots nroots)
    ;; scheme_program: main, the globals and their count, the literals and their count, the
    ;; embedded files, the display flag
    (forward-words (load (address "scheme_program") (raw 1)) (load (address "scheme_program") (raw 2)))
    (forward-words (load (address "scheme_program") (raw 3)) (load (address "scheme_program") (raw 4)))
    (forward-words (address "scheme_library_literals")
                   (load (address "scheme_library_nliterals") (raw 0)))
    (forward-words symbol-table (raw 1))
    (forward-words current-ports (raw 3))
    (forward-words output-ports (raw 1))
    (let scan-next ((p to))
      (if (raw<u p (load gc-free (raw 0)))
          (scan-next (raw+ p (raw<< (scan p) (raw 3))))
          undef))
    (let ((copied (raw>> (raw- (load gc-free (raw 0)) to) (raw 3)))
          (len (load scheme-len (raw 0))))
      (store gc-count (raw 0) (raw+ (load gc-count (raw 0)) (raw 1)))
      (store gc-copied (raw 0) (raw+ (load gc-copied (raw 0)) copied))
      (store gc-used-before (raw 0) (raw+ (load gc-used-before (raw 0)) len))
      (when (raw>u copied (load gc-max-live (raw 0)))
        (store gc-max-live (raw 0) copied))
      ;; Fresh mappings are zero
      (if grow?
          (begin
            (unmap-words (load scheme-mem (raw 0)) cap)
            (unmap-words (load spare-mem (raw 0)) cap)
            (store spare-mem (raw 0) (map-words target))
            (set-capacity! target))
          (begin
            (fill-words (load scheme-mem (raw 0)) len (raw 0))
            (store spare-mem (raw 0) (load scheme-mem (raw 0)))))
      (store scheme-mem (raw 0) to)
      (store scheme-len (raw 0) copied)
      (let ((cap (load scheme-cap (raw 0))))
        (when (raw>u (raw<< copied (raw 2)) cap)
          (grow-target! (raw<< cap (raw 1))))))))


;; The size of both spaces, which the stubs' fast path allocates up to (but in stress mode)
(define (set-capacity! (raw n))
  (declare unsafe noalloc (result void))
  (store scheme-cap (raw 0) n)
  (when (raw= (load gc-stress (raw 0)) (raw 0))
    (store scheme-limit (raw 0) n)))


;; The next collection's to-space: n words, at most max-heap-words
(define (grow-target! (raw n))
  (declare unsafe noalloc (result void))
  (store gc-target (raw 0) (if (raw>u n max-heap-words) max-heap-words n)))

(define (free-words)
  (declare unsafe inline (result raw))
  (raw- (load scheme-cap (raw 0)) (load scheme-len (raw 0))))

;; nwords words of the current space, after a collection if it is full, and a second one
;; into larger spaces if they still do not fit; called from the stubs in asm.sls, the only
;; place where a collection may happen
(define (alloc-words (raw nwords) (raw roots) (raw nroots))
  (declare unsafe noalloc (result raw))
  (when (or (gc-stress-due?) (raw>u nwords (free-words)))
    (collect roots nroots)
    (when (raw>u nwords (free-words))
      (let ((need (raw+ (load scheme-len (raw 0)) nwords)))
        (when (raw>u need max-heap-words)
          (fatal "Out of memory\n"))
        (grow-target! (let double ((n (raw<< (load scheme-cap (raw 0)) (raw 1))))
                        (if (raw<u n need) (double (raw<< n (raw 1))) n)))
        (collect roots nroots))))
  (let ((len (load scheme-len (raw 0))))
    (store scheme-len (raw 0) (raw+ len nwords))
    (raw+ (load scheme-mem (raw 0)) (raw<< len (raw 3)))))
