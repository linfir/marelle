;; GC state; the stubs in asm.sls use the words with a c-name
(define-data scheme-mem 1 (c-name "scheme_mem"))
(define-data scheme-len 1 (c-name "scheme_len"))
(define-data scheme-cap 1)
(define-data scheme-limit 1 (c-name "scheme_limit"))
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

;; Writes the nonnegative n in base radix, at most 16, with uppercase letters
(define (put-digits (raw fd) (raw n) (raw radix))
  (declare native (result void))
  (let digit-next ((m n)
                   (i (raw 24)))
    (let ((j (raw- i (raw 1)))
          (d (raw% m radix)))
      (store8 digits j (raw+ d (if (raw< d (raw 10)) (raw 48) (raw 55)))) ; 0, A - 10
      (if (raw= (raw/ m radix) (raw 0))
          (write-bytes fd (raw+ digits j) (raw- (raw 24) j))
          (digit-next (raw/ m radix) j)))))

(define (put-int (raw fd) (raw n))
  (declare native (result void))
  (when (raw< n (raw 0))
    (put-string fd "-"))
  (put-digits fd (if (raw< n (raw 0)) (raw- (raw 0) n) n) (raw 10)))

;; Writes the UTF-8 encoding of the code point n
(define (put-utf8 (raw fd) (raw n))
  (declare native (result void))
  (let ((nbytes (if (raw<u n (raw 128))
                    (raw 1)
                    (if (raw<u n (raw 2048))
                        (raw 2)
                        (if (raw<u n (raw 65536)) (raw 3) (raw 4))))))
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
  (if (fixnum? x)
      (put-int out (raw>> (as-raw x) (raw 3)))
      (if (tag=? x (raw 2))
          (if (char? x)
              (display-char out x)
              (put-string out
                          (if (eq? x #f)
                              "#f"
                              (if (eq? x #t)
                                  "#t"
                                  (if (null? x)
                                      "()"
                                      (if (eq? x undef) "#<undef>" "#<err>"))))))
          (if (pair? x)
              (display-pair out x)
              (if (tag=? x (raw 0))
                  (display-pointer out x)
                  (put-string out "#<err>"))))))

;; The name of the character n, or 0
(define (char-name (raw n))
  (declare native (result raw))
  (if (raw= n (raw 0)) "nul"
      (if (raw= n (raw 7)) "alarm"
          (if (raw= n (raw 8)) "backspace"
              (if (raw= n (raw 9)) "tab"
                  (if (raw= n (raw 10)) "newline"
                      (if (raw= n (raw 11)) "vtab"
                          (if (raw= n (raw 12)) "page"
                              (if (raw= n (raw 13)) "return"
                                  (if (raw= n (raw 27)) "esc"
                                      (if (raw= n (raw 32)) "space"
                                          (if (raw= n (raw 127)) "delete"
                                              (raw 0)))))))))))))

;; Prints c like Chez's write: a name, hexadecimal for the other control
;; characters and the line separators U+0085 and U+2028, else UTF-8
(define (display-char (raw out) c)
  (declare native (result void))
  (let* ((n (raw>> (as-raw c) (raw 8)))
         (name (char-name n)))
    (put-string out "#\\")
    (if (not (raw= name (raw 0)))
        (put-string out name)
        (if (or (raw< n (raw 32)) (raw= n (raw 133)) (raw= n (raw 8232)))
            (begin
              (put-string out "x")
              (put-digits out n (raw 16)))
            (put-utf8 out n)))))

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
  (if (raw= (as-raw x) (raw 0))
      (put-string out "#<null>")
      (if (raw= (load (as-raw x) (raw 0)) (procedure-type))
          (let ((name (load (as-raw x) (raw 3))))
            (if (raw= name (raw 0))
                (put-string out "#<procedure>")
                (begin
                  (put-string out "#<procedure ")
                  (put-string out name)
                  (put-string out ">"))))
          (put-string out "#<unknown ptr>"))))

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
    (if (raw= (load8 name i) (raw 0))
        (if (raw= (load8 entry i) (raw 61)) ; =
            (raw+ entry (raw+ i (raw 1)))
            (raw 0))
        (if (raw= (load8 entry i) (load8 name i))
            (compare-next (raw+ i (raw 1)))
            (raw 0)))))

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
      (if (raw= (load8 s i) (raw 0))
          n
          (if (or (raw>u d (raw 9))
                  (raw>u n (raw 1844674407370955161)) ; (2^64 - 1) / 10
                  (and (raw= n (raw 1844674407370955161)) (raw>u d (raw 5))))
              (raw 0)
              (digit-next (raw+ i (raw 1)) (raw+ (raw* n (raw 10)) d)))))))

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
  (if (raw= (load p (raw 0)) (frame-type))
      (load p (raw 1))
      (if (raw= (load p (raw 0)) (procedure-type))
          (raw+ (load p (raw 4)) (raw 5))
          (raw 2)))) ; pair or box

;; The new address of the object x points to, copied to gc-free if it is in the
;; current space; a moved object holds forward-type and its new address
(define (forward (raw x))
  (declare native (result raw))
  (let ((t (bit-and x (raw 7))))
    (if (not (or (raw= t (raw 0)) (raw= t (raw 3))))
        x
        (let ((p (bit-and x (raw -8)))
              (mem (load scheme-mem (raw 0))))
          (if (or (raw<u p mem)
                  (raw>=u p (raw+ mem (raw<< (load scheme-len (raw 0)) (raw 3)))))
              x ; null or static procedure
              (if (raw= (load p (raw 0)) (forward-type))
                  (bit-or (load p (raw 1)) t)
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
         (start (if frame?
                    (raw 4)
                    (if (raw= header (procedure-type))
                        (raw 5)
                        (if (raw= header (box-type)) (raw 1) (raw 0))))))
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
  (declare native (result raw) (c-name "scheme_alloc"))
  (when (or (gc-stress-due?) (raw>u nwords (free-words)))
    (collect roots nroots)
    (when (raw>u nwords (free-words))
      (fatal "Out of memory\n")))
  (let ((len (load scheme-len (raw 0))))
    (store scheme-len (raw 0) (raw+ len nwords))
    (raw+ (load scheme-mem (raw 0)) (raw<< len (raw 3)))))

;; Called from _start with the initial stack: argc, the arguments, 0, then the environment
(define (init-runtime (raw sp))
  (declare native (result void) (c-name "scheme_init"))
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
  (declare native (result never) (c-name "scheme_finish"))
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

(define (check-fixnums x y (raw msg))
  (declare inline (result void))
  (unless (fixnum? x)
    (scheme-error msg x))
  (unless (fixnum? y)
    (scheme-error msg y)))

;; Tagged fixnums: (n << 3) | 1, so the tags cancel out
(define (+ x y)
  (declare inline public)
  (check-fixnums x y "`+` expected an integer")
  (as-val (raw- (raw+ (as-raw x) (as-raw y)) (raw 1))))

(define (- x y)
  (declare inline public)
  (check-fixnums x y "`-` expected an integer")
  (as-val (raw+ (raw- (as-raw x) (as-raw y)) (raw 1))))

(define (* x y)
  (declare inline public)
  (check-fixnums x y "`*` expected an integer")
  (as-val (raw+ (raw* (raw- (as-raw x) (raw 1)) (raw>> (as-raw y) (raw 3)))
                (raw 1))))

;; Comparing tagged fixnums compares their values
(define (= x y)
  (declare inline public)
  (check-fixnums x y "`=` expected an integer")
  (raw= (as-raw x) (as-raw y)))

(define (< x y)
  (declare inline public)
  (check-fixnums x y "`<` expected an integer")
  (raw< (as-raw x) (as-raw y)))

(define (<= x y)
  (declare inline public)
  (check-fixnums x y "`<=` expected an integer")
  (raw<= (as-raw x) (as-raw y)))

(define (> x y)
  (declare inline public)
  (check-fixnums x y "`>` expected an integer")
  (raw> (as-raw x) (as-raw y)))

(define (>= x y)
  (declare inline public)
  (check-fixnums x y "`>=` expected an integer")
  (raw>= (as-raw x) (as-raw y)))

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
