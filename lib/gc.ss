;; GC state; the stubs in asm.sls use scheme-mem, scheme-len and scheme-limit
(define-data scheme-mem 1)
(define-data scheme-len 1)
(define-data scheme-cap 1)
(define-data scheme-limit 1)
(define-data spare-mem 1)
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
  (declare unsafe native (result void))
  (put-string (stderr) "GC: ")
  (put-int (stderr) (load gc-count (raw 0)))
  (put-string (stderr) " collections, ")
  (put-int (stderr) (raw- (raw+ (load gc-used-before (raw 0)) (load scheme-len (raw 0)))
                          (load gc-copied (raw 0))))
  (put-string (stderr) " words allocated, ")
  (put-int (stderr) (load gc-copied (raw 0)))
  (put-string (stderr) " words copied, ")
  (put-int (stderr) (load gc-max-live (raw 0)))
  (put-string (stderr) " max live words\n"))

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
;; scheme-mem is the current one, scheme-len its used words out of scheme-cap;
;; each space is zero past its used part, so frame slots not yet written hold 0

(define (object-words (raw p))
  (declare unsafe native (result raw))
  (cond
   ((raw= (load p (raw 0)) (frame-type)) (load p (raw 1)))
   ((raw= (load p (raw 0)) (procedure-type)) (raw+ (load p (raw 4)) (raw 5)))
   ((raw= (load p (raw 0)) (bignum-type)) (raw+ (load p (raw 1)) (raw 3)))
   ((raw= (load p (raw 0)) (vector-type)) (raw+ (load p (raw 1)) (raw 2)))
   ((raw= (load p (raw 0)) (flexvector-type)) (raw 3))
   (else (raw 2)))) ; pair or box

;; The new address of the object x points to, copied to gc-free if it is in the
;; current space; a moved object holds forward-type and its new address
(define (forward (raw x))
  (declare unsafe native (result raw))
  (let ((t (bit-and x (raw 7))))
    (if (not (or (raw= t (raw 0)) (raw= t (raw 3))))
        x
        (let ((p (bit-and x (raw -8)))
              (mem (load scheme-mem (raw 0))))
          (cond
           ((or (raw<u p mem)
                (raw>=u p (raw+ mem (raw<< (load scheme-len (raw 0)) (raw 3)))))
            x) ; null or static procedure
           ((raw= (load p (raw 0)) (forward-type))
            (bit-or (load p (raw 1)) t))
           (else
            (let ((new (load gc-free (raw 0)))
                  (n (if (raw= t (raw 3)) (raw 2) (object-words p))))
              (if (raw= t (raw 3))
                  (begin
                    (store new (raw 0) (load p (raw 0)))
                    (store new (raw 1) (load p (raw 1))))
                  (copy-words new p n))
              (store gc-free (raw 0) (raw+ new (raw<< n (raw 3))))
              (store p (raw 0) (forward-type))
              (store p (raw 1) new)
              (bit-or new t))))))))

(define (forward-words (raw p) (raw n))
  (declare unsafe native (result void))
  (let forward-next ((i (raw 0)))
    (if (raw= i n)
        undef
        (begin
          (store p i (forward (load p i)))
          (forward-next (raw+ i (raw 1)))))))

;; Forwards the values of the object at p in the new space, returns its length;
;; the raw slots of a frame are not scanned
(define (scan (raw p))
  (declare unsafe native (result raw))
  (let* ((n (object-words p))
         (header (load p (raw 0)))
         (frame? (raw= header (frame-type)))
         (start (cond
                 (frame? (raw 4))
                 ((raw= header (procedure-type)) (raw 5))
                 ((raw= header (box-type)) (raw 1))
                 ((raw= header (bignum-type)) n) ; only raw words
                 ((raw= header (vector-type)) (raw 2))
                 ((raw= header (flexvector-type)) (raw 2)) ; the storage
                 (else (raw 0)))))
    (forward-words (raw+ p (raw<< start (raw 3)))
                   (raw- (if frame? (raw- n (load p (raw 2))) n) start))
    n))

;; Moves the live objects and updates the roots, which must hold every heap pointer in use;
;; the rest is reached through them
(define (collect (raw roots) (raw nroots))
  (declare unsafe native (result void))
  (let ((to (load spare-mem (raw 0))))
    (store gc-free (raw 0) to)
    (forward-words roots nroots)
    (forward-words (address "scheme_globals") (load (address "scheme_nglobals") (raw 0)))
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
      (fill-words (load scheme-mem (raw 0)) len (raw 0))
      (store spare-mem (raw 0) (load scheme-mem (raw 0)))
      (store scheme-mem (raw 0) to)
      (store scheme-len (raw 0) copied))))

(define (free-words)
  (declare unsafe inline (result raw))
  (raw- (load scheme-cap (raw 0)) (load scheme-len (raw 0))))

;; nwords words of the current space, after a collection if it is full;
;; called from the stubs in asm.sls, the only place where a collection may happen
(define (alloc-words (raw nwords) (raw roots) (raw nroots))
  (declare unsafe native (result raw))
  (when (or (gc-stress-due?) (raw>u nwords (free-words)))
    (collect roots nroots)
    (when (raw>u nwords (free-words))
      (fatal "Out of memory\n")))
  (let ((len (load scheme-len (raw 0))))
    (store scheme-len (raw 0) (raw+ len nwords))
    (raw+ (load scheme-mem (raw 0)) (raw<< len (raw 3)))))
