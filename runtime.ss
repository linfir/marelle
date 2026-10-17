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
;; The environment variables, NAME=VALUE C strings ending with 0
(define-data environment 1)
;; Nonzero when SCHEME_GC_STATS is set and nonempty
(define-data gc-stats 1)


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

(define (pair? x)
  (declare inline public)
  (tag=? x (raw 3)))

(define (stdout)
  (declare inline (result raw))
  (raw 1))

(define (stderr)
  (declare inline (result raw))
  (raw 2))

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

;; Writes the n bytes at buf to the file descriptor fd, stopping on an error
(define (write-bytes (raw fd) (raw buf) (raw n))
  (declare native (result void))
  (let write-next ((done (raw 0)))
    (if (raw<u done n)
        (let ((written (syscall 1 fd (raw+ buf done) (raw- n done))))
          (if (raw< written (raw 0))
              undef
              (write-next (raw+ done written))))
        undef)))

(define (c-string-length (raw s))
  (declare native (result raw))
  (let length-next ((i (raw 0)))
    (if (raw= (load8 s i) (raw 0))
        i
        (length-next (raw+ i (raw 1))))))

(define (put-string (raw fd) (raw s))
  (declare native (result void))
  (write-bytes fd s (c-string-length s)))

;; Room for the digits of any raw, written backwards
(define-data digits 3)

;; Writes the nonnegative n in base radix, at most 16, with uppercase letters,
;; zero-padded to width digits (at most 24)
(define (put-digits (raw fd) (raw n) (raw radix) (raw width))
  (declare native (result void))
  (let digit-next ((m n)
                   (i (raw 24)))
    (let ((j (raw- i (raw 1)))
          (d (raw% m radix)))
      (store8 digits j (raw+ d (if (raw< d (raw 10)) (raw 48) (raw 55)))) ; 0, A - 10
      (if (and (raw= (raw/ m radix) (raw 0)) (raw>= (raw- (raw 24) j) width))
          (write-bytes fd (raw+ digits j) (raw- (raw 24) j))
          (digit-next (raw/ m radix) j)))))

(define (put-int (raw fd) (raw n))
  (declare native (result void))
  (when (raw< n (raw 0))
    (put-string fd "-"))
  (put-digits fd (if (raw< n (raw 0)) (raw- (raw 0) n) n) (raw 10) (raw 1)))

;; Writes the UTF-8 encoding of the code point n
(define (put-utf8 (raw fd) (raw n))
  (declare native (result void))
  (let ((nbytes (cond
                 ((raw<u n (raw 128)) (raw 1))
                 ((raw<u n (raw 2048)) (raw 2))
                 ((raw<u n (raw 65536)) (raw 3))
                 (else (raw 4)))))
    (let byte-next ((m n)
                    (i (raw- nbytes (raw 1))))
      (if (raw= i (raw 0))
          (begin
            ;; The lead byte: 0, else 0xFF00 >> nbytes is 0xC0, 0xE0 or 0xF0
            (store8 digits i (if (raw= nbytes (raw 1))
                                 m
                                 (bit-or m (bit-and (raw>> (raw 65280) nbytes) (raw 255)))))
            (write-bytes fd digits nbytes))
          (begin
            (store8 digits i (bit-or (bit-and m (raw 63)) (raw 128)))
            (byte-next (raw>> m (raw 6)) (raw- i (raw 1))))))))

;; Prints x to the file descriptor out
(define (display (raw out) x)
  (declare native (result void))
  (cond
   ((fixnum? x) (put-int out (raw>> (as-raw x) (raw 3))))
   ((char? x) (display-char out x))
   ((tag=? x (raw 2))
    (put-string out (cond
                     ((eq? x #f) "#f")
                     ((eq? x #t) "#t")
                     ((null? x) "()")
                     ((eq? x undef) "#<undef>")
                     (else "#<err>"))))
   ((pair? x) (display-pair out x))
   ((tag=? x (raw 0)) (display-pointer out x))
   (else (put-string out "#<err>"))))

;; The name of the character n, or 0
(define (char-name (raw n))
  (declare native (result raw))
  (cond
   ((raw= n (raw 0))   "nul")
   ((raw= n (raw 7))   "alarm")
   ((raw= n (raw 8))   "backspace")
   ((raw= n (raw 9))   "tab")
   ((raw= n (raw 10))  "newline")
   ((raw= n (raw 11))  "vtab")
   ((raw= n (raw 12))  "page")
   ((raw= n (raw 13))  "return")
   ((raw= n (raw 27))  "esc")
   ((raw= n (raw 32))  "space")
   ((raw= n (raw 127)) "delete")
   (else (raw 0))))

;; Prints c like Chez's write: a name, hexadecimal for the other control
;; characters and the line separators U+0085 and U+2028, else UTF-8
(define (display-char (raw out) c)
  (declare native (result void))
  (let* ((n (raw>> (as-raw c) (raw 8)))
         (name (char-name n)))
    (put-string out "#\\")
    (cond
     ((not (raw= name (raw 0))) (put-string out name))
     ((or (raw< n (raw 32)) (raw= n (raw 133)) (raw= n (raw 8232)))
      (put-string out "x")
      (put-digits out n (raw 16) (raw 1)))
     (else (put-utf8 out n)))))

(define (display-pair (raw out) x)
  (declare native (result void))
  (put-string out "(")
  (display out (car x))
  (let more ((rest (cdr x)))
    (if (pair? rest)
        (begin
          (put-string out " ")
          (display out (car rest))
          (more (cdr rest)))
        (begin
          (unless (null? rest)
            (put-string out " . ")
            (display out rest))
          (put-string out ")")))))

(define (display-pointer (raw out) x)
  (declare native (result void))
  (cond
   ((raw= (as-raw x) (raw 0)) (put-string out "#<null>"))
   ((raw= (load (as-raw x) (raw 0)) (bignum-type)) (display-bignum out (as-raw x)))
   ((raw= (load (as-raw x) (raw 0)) (procedure-type))
    (let ((name (load (as-raw x) (raw 3))))
      (if (raw= name (raw 0))
          (put-string out "#<procedure>")
          (begin
            (put-string out "#<procedure ")
            (put-string out name)
            (put-string out ">")))))
   (else (put-string out "#<unknown ptr>"))))

;; Divides the m limbs at q by 10^9 in place, returns the remainder; each limb is two
;; 32-bit halves, so rem << 32 | half < 2^62 and the signed raw/ works
(define (divide-limbs! (raw q) (raw m))
  (declare native (result raw))
  (let limb-next ((i m)
                  (rem (raw 0)))
    (if (raw= i (raw 0))
        rem
        (let* ((limb (load q (raw- i (raw 1))))
               (hi (bit-or (raw<< rem (raw 32)) (raw>>u limb (raw 32))))
               (lo (bit-or (raw<< (raw% hi (raw 1000000000)) (raw 32))
                           (bit-and limb (raw 4294967295)))))
          (store q (raw- i (raw 1)) (bit-or (raw<< (raw/ hi (raw 1000000000)) (raw 32))
                                            (raw/ lo (raw 1000000000))))
          (limb-next (raw- i (raw 1)) (raw% lo (raw 1000000000)))))))

;; The number of limbs at q once its leading zero limbs are dropped
(define (significant-limbs (raw q) (raw m))
  (declare native (result raw))
  (if (and (raw>u m (raw 0)) (raw= (load q (raw- m (raw 1))) (raw 0)))
      (significant-limbs q (raw- m (raw 1)))
      m))

;; Prints the bignum at x in decimal: its limbs, copied to spare-mem (unused outside a
;; collection), are divided by 10^9 until zero, the remainders stored after them, then printed
;; most significant first, 9 digits each but the first; spare-mem is zeroed again after
(define (display-bignum (raw out) (raw x))
  (declare native (result void))
  (let* ((n (load x (raw 1)))
         (q (load spare-mem (raw 0)))
         (chunks (raw+ q (raw<< n (raw 3)))))
    ;; A chunk per 29.9 bits: at most 2.2 n + 1 of them
    (when (raw>u (raw+ (raw<< n (raw 2)) (raw 1)) (load scheme-cap (raw 0)))
      (fatal "Out of memory\n"))
    (when (raw= (load x (raw 2)) (raw 1))
      (put-string out "-"))
    (copy-words q (raw+ x (raw 24)) n)
    (let chunk-next ((k (raw 0))
                     (m n))
      (if (raw>u m (raw 0))
          (begin
            (store chunks k (divide-limbs! q m))
            (chunk-next (raw+ k (raw 1)) (significant-limbs q m)))
          (begin
            (put-digits out (load chunks (raw- k (raw 1))) (raw 10) (raw 1))
            (let print-next ((i (raw- k (raw 1))))
              (if (raw= i (raw 0))
                  undef
                  (begin
                    (put-digits out (load chunks (raw- i (raw 1))) (raw 10) (raw 9))
                    (print-next (raw- i (raw 1))))))
            (fill-words q (raw+ n k) (raw 0)))))))

;; Prints the statistics on stderr when SCHEME_GC_STATS is set, then exits
(define (exit-process (raw status))
  (declare native (result never))
  (unless (raw= (load gc-stats (raw 0)) (raw 0))
    (print-gc-stats))
  (syscall 231 status)) ; exit_group

;; Each stretch between collections allocates from the words copied up to the used words
(define (print-gc-stats)
  (declare native (result void))
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

;; Prints msg and exits, when the program cannot go on
(define (fatal (raw msg))
  (declare native (result never))
  (put-string (stderr) msg)
  (exit-process (raw 1)))

;; The address of the value of entry, a NAME=VALUE string, or 0 if its name is not name
(define (env-value (raw entry) (raw name))
  (declare native (result raw))
  (let compare-next ((i (raw 0)))
    (cond
     ((raw= (load8 name i) (raw 0))
      (if (raw= (load8 entry i) (raw 61)) ; =
          (raw+ entry (raw+ i (raw 1)))
          (raw 0)))
     ((raw= (load8 entry i) (load8 name i)) (compare-next (raw+ i (raw 1))))
     (else (raw 0)))))

;; The value of the environment variable name, or 0 if unset
(define (getenv (raw name))
  (declare native (result raw))
  (let env-next ((k (raw 0)))
    (let ((entry (load (load environment (raw 0)) k)))
      (if (raw= entry (raw 0))
          (raw 0)
          (let ((value (env-value entry name)))
            (if (raw= value (raw 0))
                (env-next (raw+ k (raw 1)))
                value))))))

;; The decimal number s, or 0 if it is empty, has a non-digit or overflows
(define (parse-positive (raw s))
  (declare native (result raw))
  (let digit-next ((i (raw 0))
                   (n (raw 0)))
    (let ((d (raw- (load8 s i) (raw 48))))
      (cond
       ((raw= (load8 s i) (raw 0)) n)
       ((or (raw>u d (raw 9))
            (raw>u n (raw 1844674407370955161)) ; (2^64 - 1) / 10
            (and (raw= n (raw 1844674407370955161)) (raw>u d (raw 5))))
        (raw 0))
       (else (digit-next (raw+ i (raw 1)) (raw+ (raw* n (raw 10)) d)))))))

;; The positive integer in the environment variable name, or default if unset;
;; exits if it is not a positive decimal integer up to max
(define (env-positive (raw name) (raw default) (raw max))
  (declare native (result raw))
  (let ((s (getenv name)))
    (if (raw= s (raw 0))
        default
        (let ((n (parse-positive s)))
          (when (or (raw= n (raw 0)) (raw>u n max))
            (put-string (stderr) "Invalid ")
            (put-string (stderr) name)
            (put-string (stderr) ": ")
            (put-string (stderr) s)
            (put-string (stderr) "\n")
            (exit-process (raw 1)))
          n))))

;; n zeroed words, touched lazily
(define (map-words (raw n))
  (declare native (result raw))
  ;; mmap(0, 8n, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0)
  (let ((p (syscall 9 (raw 0) (raw<< n (raw 3)) (raw 3) (raw 34) (raw -1) (raw 0))))
    (when (raw>u p (raw -4096))
      (fatal "Memory allocation failed\n"))
    p))

;; Counts down to the next forced collection in stress mode (SCHEME_GC_STRESS)
(define (gc-stress-due?)
  (declare inline)
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
  (declare native (result raw))
  (cond
   ((raw= (load p (raw 0)) (frame-type)) (load p (raw 1)))
   ((raw= (load p (raw 0)) (procedure-type)) (raw+ (load p (raw 4)) (raw 5)))
   ((raw= (load p (raw 0)) (bignum-type)) (raw+ (load p (raw 1)) (raw 3)))
   (else (raw 2)))) ; pair or box

;; The new address of the object x points to, copied to gc-free if it is in the
;; current space; a moved object holds forward-type and its new address
(define (forward (raw x))
  (declare native (result raw))
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
  (declare native (result void))
  (let forward-next ((i (raw 0)))
    (if (raw= i n)
        undef
        (begin
          (store p i (forward (load p i)))
          (forward-next (raw+ i (raw 1)))))))

;; Forwards the values of the object at p in the new space, returns its length;
;; the raw slots of a frame are not scanned
(define (scan (raw p))
  (declare native (result raw))
  (let* ((n (object-words p))
         (header (load p (raw 0)))
         (frame? (raw= header (frame-type)))
         (start (cond
                 (frame? (raw 4))
                 ((raw= header (procedure-type)) (raw 5))
                 ((raw= header (box-type)) (raw 1))
                 ((raw= header (bignum-type)) n) ; only raw words
                 (else (raw 0)))))
    (forward-words (raw+ p (raw<< start (raw 3)))
                   (raw- (if frame? (raw- n (load p (raw 2))) n) start))
    n))

;; Moves the live objects and updates the roots, which must hold every heap pointer in use;
;; the rest is reached through them
(define (collect (raw roots) (raw nroots))
  (declare native (result void))
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
  (declare inline (result raw))
  (raw- (load scheme-cap (raw 0)) (load scheme-len (raw 0))))

;; nwords words of the current space, after a collection if it is full;
;; called from the stubs in asm.sls, the only place where a collection may happen
(define (alloc-words (raw nwords) (raw roots) (raw nroots))
  (declare native (result raw))
  (when (or (gc-stress-due?) (raw>u nwords (free-words)))
    (collect roots nroots)
    (when (raw>u nwords (free-words))
      (fatal "Out of memory\n")))
  (let ((len (load scheme-len (raw 0))))
    (store scheme-len (raw 0) (raw+ len nwords))
    (raw+ (load scheme-mem (raw 0)) (raw<< len (raw 3)))))

;; Called from _start with the initial stack: argc, the arguments, 0, then the environment
(define (init-runtime (raw sp))
  (declare native (result void))
  (store environment (raw 0) (raw+ sp (raw<< (raw+ (load sp (raw 0)) (raw 2)) (raw 3))))
  ;; 2^26 words by default: 1 GiB for both spaces
  (store scheme-cap (raw 0)
         (env-positive "SCHEME_HEAP" (raw 67108864) (raw 2305843009213693951)))
  (store gc-stress (raw 0)
         (env-positive "SCHEME_GC_STRESS" (raw 0) (raw 18446744073709551615)))
  (store gc-countdown (raw 0) (load gc-stress (raw 0)))
  (store scheme-limit (raw 0)
         (if (raw= (load gc-stress (raw 0)) (raw 0)) (load scheme-cap (raw 0)) (raw 0)))
  (let ((stats (getenv "SCHEME_GC_STATS")))
    (when (and (not (raw= stats (raw 0)))
               (not (raw= (load8 stats (raw 0)) (raw 0))))
      (store gc-stats (raw 0) (raw 1))))
  (store scheme-mem (raw 0) (map-words (load scheme-cap (raw 0))))
  (store spare-mem (raw 0) (map-words (load scheme-cap (raw 0)))))

;; Called from _start with the value of scheme_main
(define (finish-program v)
  (declare native (result never))
  (display (stdout) v)
  (exit-process (raw 0)))

;; Prints "Error: msg: x" and exits
(define (scheme-error (raw msg) x)
  (declare native (result never))
  (put-string (stderr) "Error: ")
  (put-string (stderr) msg)
  (put-string (stderr) ": ")
  (display (stderr) x)
  (put-string (stderr) "\n")
  (exit-process (raw 1)))

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

(define (car x)
  (declare inline public)
  (unless (pair? x)
    (scheme-error "`car` expected a pair" x))
  (as-val (load (untag x) (raw 0))))

(define (cdr x)
  (declare inline public)
  (unless (pair? x)
    (scheme-error "`cdr` expected a pair" x))
  (as-val (load (untag x) (raw 1))))

(define (cons a d)
  (declare inline public)
  (let ((p (alloc (raw 2))))
    (store p (raw 0) a)
    (store p (raw 1) d)
    (as-val (bit-or p (raw 3)))))

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

;; Integers: fixnums, (n << 3) | 1 for n in [-2^60, 2^60), else bignums
;; [bignum-type, nlimbs, sign, limb 0, ..., limb nlimbs - 1]: raw words, little-endian unsigned
;; 64-bit limbs, sign 0 or 1; normalized, so no leading zero limb and never in the fixnum range

(define (bignum? x)
  (declare inline)
  (and (tag=? x (raw 0))
       (not (raw= (as-raw x) (raw 0)))
       (raw= (load (as-raw x) (raw 0)) (bignum-type))))

(define (check-integer x (raw msg))
  (declare inline (result void))
  (unless (or (fixnum? x) (bignum? x))
    (scheme-error msg x)))

(define (fixnum-value x)
  (declare inline (result raw))
  (raw>> (as-raw x) (raw 3)))

(define (make-fixnum (raw n))
  (declare inline)
  (as-val (bit-or (raw<< n (raw 3)) (raw 1))))

;; A tagged fixnum is its value times 8, plus 1, so the fixnum range is the signed word range:
;; a tagged sum overflows when the 64-bit sum of the words does, and that one
;; when the operands have the same sign and the result the other
(define (sum-overflow? (raw a) (raw b) (raw s))
  (declare inline)
  (if (raw< a (raw 0))
      (and (raw< b (raw 0)) (raw>= s (raw 0)))
      (and (raw>= b (raw 0)) (raw< s (raw 0)))))

;; a - b: when the operands have different signs and the result has b's
(define (difference-overflow? (raw a) (raw b) (raw d))
  (declare inline)
  (if (raw< a (raw 0))
      (and (raw>= b (raw 0)) (raw>= d (raw 0)))
      (and (raw< b (raw 0)) (raw< d (raw 0)))))

;; The value of the fixnum x is in (-2^30, 2^30), so the product of two fits a fixnum:
;; its tagged word is in [-(2^30 - 1) * 8 + 1, (2^30 - 1) * 8 + 1]
(define (small-fixnum? x)
  (declare inline)
  (raw<u (raw+ (as-raw x) (raw 8589934583)) (raw 17179869169)))

;; Limb i of the bignum at p with n limbs, 0 past them
(define (limb (raw p) (raw n) (raw i))
  (declare inline (result raw))
  (if (raw<u i n) (load p (raw+ i (raw 3))) (raw 0)))

;; The 1-limb bignum of the fixnum x, not normalized
(define (fixnum->bignum x)
  (let ((p (alloc (raw 4))))
    (store p (raw 0) (bignum-type))
    (store p (raw 1) (raw 1))
    (store p (raw 2) (if (raw< (fixnum-value x) (raw 0)) (raw 1) (raw 0)))
    (store p (raw 3) (if (raw< (fixnum-value x) (raw 0))
                         (raw- (raw 0) (fixnum-value x))
                         (fixnum-value x)))
    (as-val p)))

(define (->bignum x)
  (declare inline)
  (if (fixnum? x) (fixnum->bignum x) x))

;; -1, 0 or 1 as the magnitude of the bignum a is below, equal to or above b's;
;; a 1-limb operand may have a zero limb
(define (compare-magnitudes (raw a) (raw b))
  (declare native (result raw))
  (let ((na (load a (raw 1)))
        (nb (load b (raw 1))))
    (cond
     ((raw<u na nb) (raw -1))
     ((raw>u na nb) (raw 1))
     (else
      (let limb-next ((i na))
        (if (raw= i (raw 0))
            (raw 0)
            (let ((x (load a (raw+ i (raw 2))))
                  (y (load b (raw+ i (raw 2)))))
              (cond
               ((raw<u x y) (raw -1))
               ((raw>u x y) (raw 1))
               (else (limb-next (raw- i (raw 1))))))))))))

;; -1, 0 or 1 as x is below, equal to or above y; both normalized integers, else an error;
;; a bignum is beyond every fixnum, so its sign decides against one
(define (compare-integers x y (raw msg))
  (declare native (result raw))
  (check-integer x msg)
  (check-integer y msg)
  (cond
   ((and (fixnum? x) (fixnum? y))
    (cond
     ((raw< (as-raw x) (as-raw y)) (raw -1))
     ((raw= (as-raw x) (as-raw y)) (raw 0))
     (else (raw 1))))
   ((fixnum? x) (if (raw= (load (as-raw y) (raw 2)) (raw 1)) (raw 1) (raw -1)))
   ((fixnum? y) (if (raw= (load (as-raw x) (raw 2)) (raw 1)) (raw -1) (raw 1)))
   (else
    (let ((sx (load (as-raw x) (raw 2)))
          (sy (load (as-raw y) (raw 2))))
      (cond
       ((raw< sx sy) (raw 1))
       ((raw> sx sy) (raw -1))
       ((raw= sx (raw 0)) (compare-magnitudes (as-raw x) (as-raw y)))
       (else (compare-magnitudes (as-raw y) (as-raw x))))))))

;; The limbs of r: |a| + |b|, max(na, nb) + 1 of them
(define (add-magnitudes! (raw r) (raw a) (raw b))
  (declare native (result void))
  (let* ((na (load a (raw 1)))
         (nb (load b (raw 1)))
         (n (if (raw>u na nb) na nb)))
    (store r (raw 1) (raw+ n (raw 1)))
    (let limb-next ((i (raw 0))
                    (carry (raw 0)))
      (if (raw= i n)
          (store r (raw+ i (raw 3)) carry)
          (let* ((x (limb a na i))
                 (s (raw+ x (limb b nb i)))
                 (t (raw+ s carry)))
            (store r (raw+ i (raw 3)) t)
            (limb-next (raw+ i (raw 1))
                       (raw+ (if (raw<u s x) (raw 1) (raw 0))
                             (if (raw<u t s) (raw 1) (raw 0)))))))))

;; The limbs of r: |a| - |b|, with |a| >= |b|, na of them
(define (subtract-magnitudes! (raw r) (raw a) (raw b))
  (declare native (result void))
  (let ((na (load a (raw 1)))
        (nb (load b (raw 1))))
    (store r (raw 1) na)
    (let limb-next ((i (raw 0))
                    (borrow (raw 0)))
      (if (raw= i na)
          undef
          (let* ((x (limb a na i))
                 (y (limb b nb i))
                 (d (raw- x y)))
            (store r (raw+ i (raw 3)) (raw- d borrow))
            (limb-next (raw+ i (raw 1))
                       (raw+ (if (raw<u x y) (raw 1) (raw 0))
                             (if (raw<u d borrow) (raw 1) (raw 0)))))))))

;; The limbs of the zeroed r: |a| * |b|, na + nb of them (schoolbook);
;; x * y + r[k] + carry < 2^128, so the high word never overflows
(define (multiply-magnitudes! (raw r) (raw a) (raw b))
  (declare native (result void))
  (let ((na (load a (raw 1)))
        (nb (load b (raw 1))))
    (store r (raw 1) (raw+ na nb))
    (let row-next ((i (raw 0)))
      (if (raw= i na)
          undef
          (let ((x (load a (raw+ i (raw 3)))))
            (let column-next ((j (raw 0))
                              (carry (raw 0)))
              (if (raw= j nb)
                  (begin
                    (store r (raw+ (raw+ i nb) (raw 3)) carry)
                    (row-next (raw+ i (raw 1))))
                  (let* ((y (load b (raw+ j (raw 3))))
                         (k (raw+ (raw+ i j) (raw 3)))
                         (lo (raw* x y))
                         (lo1 (raw+ lo (load r k)))
                         (lo2 (raw+ lo1 carry)))
                    (store r k lo2)
                    (column-next (raw+ j (raw 1))
                                 (raw+ (raw+ (raw*hu x y)
                                             (if (raw<u lo1 lo) (raw 1) (raw 0)))
                                       (if (raw<u lo2 lo1) (raw 1) (raw 0))))))))))))

;; The bignum r without its leading zero limbs, or its fixnum when in range
(define (normalize (raw r))
  (declare native)
  (let limb-next ((n (load r (raw 1))))
    (if (and (raw>u n (raw 1)) (raw= (load r (raw+ n (raw 2))) (raw 0)))
        (limb-next (raw- n (raw 1)))
        (let ((top (load r (raw 3)))
              (negative? (raw= (load r (raw 2)) (raw 1))))
          (store r (raw 1) n)
          (cond
           ((not (raw= n (raw 1))) (as-val r))
           ((raw<u top (raw 1152921504606846976)) ; 2^60
            (make-fixnum (if negative? (raw- (raw 0) top) top)))
           ((and negative? (raw= top (raw 1152921504606846976)))
            (make-fixnum (raw- (raw 0) top)))
           (else (as-val r)))))))

;; r = a + b, or a - b when negate-b is 1, on bignums
(define (add-signed! (raw r) (raw a) (raw b) (raw negate-b))
  (declare native (result void))
  (let ((sa (load a (raw 2)))
        (sb (if (raw= negate-b (raw 0)) (load b (raw 2)) (raw- (raw 1) (load b (raw 2))))))
    (store r (raw 0) (bignum-type))
    (cond
     ((raw= sa sb)
      (store r (raw 2) sa)
      (add-magnitudes! r a b))
     ((raw< (compare-magnitudes a b) (raw 0))
      (store r (raw 2) sb)
      (subtract-magnitudes! r b a))
     (else
      (store r (raw 2) sa)
      (subtract-magnitudes! r a b)))))

;; a + b, or a - b when negate-b? is true; the room for a carry may stay unused
(define (add-bignums a b negate-b?)
  (let ((r (alloc (raw+ (let ((na (load (as-raw a) (raw 1)))
                              (nb (load (as-raw b) (raw 1))))
                          (if (raw>u na nb) na nb))
                        (raw 4)))))
    (add-signed! r (as-raw a) (as-raw b) (if negate-b? (raw 1) (raw 0)))
    (normalize r)))

(define (multiply-bignums a b)
  (let ((r (alloc (raw+ (raw+ (load (as-raw a) (raw 1)) (load (as-raw b) (raw 1))) (raw 3)))))
    (store r (raw 0) (bignum-type))
    (store r (raw 2) (if (raw= (load (as-raw a) (raw 2)) (load (as-raw b) (raw 2))) (raw 0) (raw 1)))
    (multiply-magnitudes! r (as-raw a) (as-raw b))
    (normalize r)))

;; The slow paths, out of line: a bignum operand or result
(define (add-integers x y)
  (check-integer x "`+` expected an integer")
  (check-integer y "`+` expected an integer")
  (add-bignums (->bignum x) (->bignum y) #f))

(define (subtract-integers x y)
  (check-integer x "`-` expected an integer")
  (check-integer y "`-` expected an integer")
  (add-bignums (->bignum x) (->bignum y) #t))

(define (multiply-integers x y)
  (check-integer x "`*` expected an integer")
  (check-integer y "`*` expected an integer")
  (multiply-bignums (->bignum x) (->bignum y)))

;; Inline fast paths on tagged fixnums: (a << 3) | 1 plus (b << 3) | 1, minus 1,
;; is ((a + b) << 3) | 1
(define (+ x y)
  (declare inline public)
  (let ((s (raw- (raw+ (as-raw x) (as-raw y)) (raw 1))))
    (if (and (fixnum? x) (fixnum? y) (not (sum-overflow? (as-raw x) (as-raw y) s)))
        (as-val s)
        (add-integers x y))))

(define (- x y)
  (declare inline public)
  (let ((d (raw- (as-raw x) (as-raw y))))
    (if (and (fixnum? x) (fixnum? y) (not (difference-overflow? (as-raw x) (as-raw y) d)))
        (as-val (raw+ d (raw 1)))
        (subtract-integers x y))))

;; (n << 3) | 1 times m is (n * m) << 3 plus m, so subtract the tag first
(define (* x y)
  (declare inline public)
  (if (and (fixnum? x) (fixnum? y) (small-fixnum? x) (small-fixnum? y))
      (as-val (raw+ (raw* (raw- (as-raw x) (raw 1)) (fixnum-value y)) (raw 1)))
      (multiply-integers x y)))

;; Comparing tagged fixnums compares their values
(define (= x y)
  (declare inline public)
  (if (and (fixnum? x) (fixnum? y))
      (raw= (as-raw x) (as-raw y))
      (raw= (compare-integers x y "`=` expected an integer") (raw 0))))

(define (< x y)
  (declare inline public)
  (if (and (fixnum? x) (fixnum? y))
      (raw< (as-raw x) (as-raw y))
      (raw< (compare-integers x y "`<` expected an integer") (raw 0))))

(define (<= x y)
  (declare inline public)
  (if (and (fixnum? x) (fixnum? y))
      (raw<= (as-raw x) (as-raw y))
      (raw<= (compare-integers x y "`<=` expected an integer") (raw 0))))

(define (> x y)
  (declare inline public)
  (if (and (fixnum? x) (fixnum? y))
      (raw> (as-raw x) (as-raw y))
      (raw> (compare-integers x y "`>` expected an integer") (raw 0))))

(define (>= x y)
  (declare inline public)
  (if (and (fixnum? x) (fixnum? y))
      (raw>= (as-raw x) (as-raw y))
      (raw>= (compare-integers x y "`>=` expected an integer") (raw 0))))

;; Characters: the immediate (code << 8) | 34
(define (char? x)
  (declare inline public)
  (raw= (bit-and (as-raw x) (raw 255)) (raw 34)))

;; ((code << 8) | 34) >> 5 is (code << 3) | 1, the fixnum code
(define (char->integer x)
  (declare inline public)
  (unless (char? x)
    (scheme-error "`char->integer` expected a character" x))
  (as-val (raw>> (as-raw x) (raw 5))))

(define (integer->char x)
  (declare inline public)
  (unless (and (fixnum? x)
               (let ((n (raw>> (as-raw x) (raw 3))))
                 (and (raw<=u n (raw 1114111))
                      (not (and (raw>= n (raw 55296)) (raw<= n (raw 57343))))))) ; surrogates
    (scheme-error "`integer->char` expected a Unicode scalar value" x))
  (as-val (bit-or (raw<< (as-raw x) (raw 5)) (raw 2))))

(define (check-chars x y (raw msg))
  (declare inline (result void))
  (unless (char? x)
    (scheme-error msg x))
  (unless (char? y)
    (scheme-error msg y)))

;; Comparing characters compares their codes
(define (char=? x y)
  (declare inline public)
  (check-chars x y "`char=?` expected a character")
  (raw= (as-raw x) (as-raw y)))

(define (char<? x y)
  (declare inline public)
  (check-chars x y "`char<?` expected a character")
  (raw< (as-raw x) (as-raw y)))

(define (char<=? x y)
  (declare inline public)
  (check-chars x y "`char<=?` expected a character")
  (raw<= (as-raw x) (as-raw y)))

(define (char>? x y)
  (declare inline public)
  (check-chars x y "`char>?` expected a character")
  (raw> (as-raw x) (as-raw y)))

(define (char>=? x y)
  (declare inline public)
  (check-chars x y "`char>=?` expected a character")
  (raw>= (as-raw x) (as-raw y)))

(define (not x)
  (declare inline public)
  (if x #f #t))
