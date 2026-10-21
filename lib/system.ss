;; The environment variables, NAME=VALUE C strings ending with 0
(define-data environment 1)

;; The initial stack: argc, then the arguments, C strings
(define-data arguments 1)

;; Writes the n bytes at buf to the file descriptor fd, stopping on an error
(define (write-all (raw fd) (raw buf) (raw n))
  (declare unsafe noalloc (result void))
  (let write-next ((done (raw 0)))
    (if (raw<u done n)
        (let ((written (syscall 1 fd (raw+ buf done) (raw- n done))))
          (if (raw< written (raw 0))
              undef
              (write-next (raw+ done written))))
        undef)))

(define (copy-bytes! (raw dst) (raw src) (raw n))
  (declare unsafe inline (result void))
  (let copy-next ((i (raw 0)))
    (if (raw<u i n)
        (begin
          (store8 dst i (load8 src i))
          (copy-next (raw+ i (raw 1))))
        undef)))

;; Writes the buffered bytes of the descriptor output port
(define (flush-port (raw port))
  (declare unsafe noalloc (result void))
  (write-all (port-fd port) (port-bytes port) (port-position port))
  (set-port-position! port (raw 0)))

;; Writes the n bytes at buf to the output port; a descriptor port's go through its buffer, and
;; a write to stdout or stderr flushes the other first, so the console keeps the order of the
;; writes; a string port has room for them (reserve!), the counting port only counts them
(define (write-bytes (raw port) (raw buf) (raw n))
  (declare unsafe noalloc (result void))
  (if (raw= (bit-and (port-flags port) string-flag) (raw 0))
      (begin
        (when (and (raw= port stdout) (not (raw= (port-position stderr) (raw 0))))
          (flush-port stderr))
        (when (and (raw= port stderr) (not (raw= (port-position stdout) (raw 0))))
          (flush-port stdout))
        (when (raw>u n (raw- port-buffer-size (port-position port)))
          (flush-port port))
        (if (raw<u n port-buffer-size)
            (let ((count (port-position port)))
              (copy-bytes! (raw+ (port-bytes port) count) buf n)
              (set-port-position! port (raw+ count n)))
            (write-all (port-fd port) buf n)))
      (let ((count (port-position port)))
        (unless (raw= port counting-port)
          (copy-bytes! (raw+ (port-bytes port) count) buf n))
        (set-port-position! port (raw+ count n)))))

(define (c-string-length (raw s))
  (declare unsafe noalloc (result raw))
  (let length-next ((i (raw 0)))
    (if (raw= (load8 s i) (raw 0))
        i
        (length-next (raw+ i (raw 1))))))

(define (put-string (raw out) (raw s))
  (declare unsafe noalloc (result void))
  (write-bytes out s (c-string-length s)))

;; Flushes the console ports and the open descriptor output ports
(define (flush-output-ports)
  (declare unsafe noalloc (result void))
  (flush-port stdout)
  (flush-port stderr)
  (let port-next ((l (as-val (load output-ports (raw 0)))))
    (if (pair? l)
        (begin
          (flush-port (as-raw (pair-car l)))
          (port-next (pair-cdr l)))
        undef)))

;; Prints the statistics on stderr when SCHEME_GC_STATS is set, flushes the output ports, then
;; exits
(define (exit-process (raw status))
  (declare unsafe noalloc (result never))
  (unless (raw= (load gc-stats (raw 0)) (raw 0))
    (print-gc-stats))
  (flush-output-ports)
  (syscall 231 status)) ; exit_group

;; Flushes the ports and exits with status 0 with no argument, a fixnum's low byte, else 1, as in
;; Chez (R7RS gives 0 for #t)
(define (exit . value)
  (declare unsafe public)
  (exit-process (cond
                 ((null? value) (raw 0))
                 ((fixnum? (car value)) (bit-and (fixnum-value (car value)) (raw 255)))
                 (else (raw 1)))))

;; Prints msg and exits, when the program cannot go on
(define (fatal (raw msg))
  (declare unsafe noalloc (result never))
  (put-string stderr msg)
  (exit-process (raw 1)))

;; The address of the value of entry, a NAME=VALUE string, or 0 if its name is not name
(define (env-value (raw entry) (raw name))
  (declare unsafe noalloc (result raw))
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
  (declare unsafe noalloc (result raw))
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
  (declare unsafe noalloc (result raw))
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
  (declare unsafe noalloc (result raw))
  (let ((s (getenv name)))
    (if (raw= s (raw 0))
        default
        (let ((n (parse-positive s)))
          (when (or (raw= n (raw 0)) (raw>u n max))
            (put-string stderr "Invalid ")
            (put-string stderr name)
            (put-string stderr ": ")
            (put-string stderr s)
            (put-string stderr "\n")
            (exit-process (raw 1)))
          n))))

;; n zeroed words, touched lazily
(define (map-words (raw n))
  (declare unsafe noalloc (result raw))
  ;; mmap(0, 8n, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0)
  (let ((p (syscall 9 (raw 0) (raw<< n (raw 3)) (raw 3) (raw 34) (raw -1) (raw 0))))
    (when (raw>u p (raw -4096))
      (fatal "Memory allocation failed\n"))
    p))

(define (unmap-words (raw p) (raw n))
  (declare unsafe noalloc (result void))
  ;; munmap(p, 8n)
  (syscall 11 p (raw<< n (raw 3)))
  undef)

;; Called from _start with the initial stack: argc, the arguments, 0, then the environment
(define (init-runtime (raw sp))
  (declare unsafe noalloc (result void))
  (init-console-ports!)
  (store arguments (raw 0) sp)
  (store environment (raw 0) (raw+ sp (raw<< (raw+ (load sp (raw 0)) (raw 2)) (raw 3))))
  (store scheme-cap (raw 0) initial-heap-words)
  (store gc-target (raw 0) initial-heap-words)
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

;; The address of argument k, a fixnum
(define (argument-address k)
  (declare unsafe inline (result raw))
  (load (load arguments (raw 0)) (raw+ (fixnum-value k) (raw 1))))

;; Argument k as a string, from its UTF-8 bytes
(define (argument-string k)
  (declare unsafe)
  (let ((bv (alloc-bytevector (make-fixnum (c-string-length (argument-address k))))))
    (copy-bytes! (byte-address bv (raw 0)) (argument-address k) (bytevector-nbytes bv))
    (utf8->string bv)))

;; The program's name and its arguments (R7RS)
(define (command-line)
  (declare unsafe public)
  (let argument-next ((k (- (make-fixnum (load (load arguments (raw 0)) (raw 0))) 1))
                      (acc '()))
    (if (< k 0)
        acc
        (argument-next (- k 1) (cons (argument-string k) acc)))))

;; Word j of entry i, a fixnum, of the table of the embedded files (scheme_program's word 5):
;; a count, then per file the address of its name (a C string), its address and its size
(define (embedded-word i (raw j))
  (declare unsafe inline (result raw))
  (load (load (address "scheme_program") (raw 5))
        (raw+ (raw+ (raw* (fixnum-value i) (raw 3)) j) (raw 1))))

;; A fresh bytevector of the n bytes at word j of entry i (a fixnum)
(define (embedded-bytes i j n)
  (declare unsafe)
  (let ((bv (alloc-bytevector n)))
    (copy-bytes! (byte-address bv (raw 0)) (embedded-word i (fixnum-value j)) (fixnum-value n))
    bv))

;; The bytes of the file schemec --embed embedded as name, a fresh bytevector, or #f
(define (embedded-file name)
  (declare unsafe public)
  (check-string name embedded-file)
  (let next ((i 0))
    (cond
     ((= i (make-fixnum (load (load (address "scheme_program") (raw 5)) (raw 0)))) #f)
     ((string=? name (utf8->string
                      (embedded-bytes i 0 (make-fixnum (c-string-length (embedded-word i (raw 0)))))))
      (embedded-bytes i 1 (make-fixnum (embedded-word i (raw 2)))))
     (else (next (+ i 1))))))

;; Called from _start with the value of scheme_main, which it prints when the program was
;; compiled with --display (scheme_program's word 6)
(define (finish-program v)
  (declare unsafe noalloc (result never))
  (unless (raw= (load (address "scheme_program") (raw 6)) (raw 0))
    (put-datum stdout (raw 1) v))
  (exit-process (raw 0)))

;; A letrec variable used or assigned before its init
(define (definition-error (raw name) (raw what))
  (declare unsafe noalloc (result never))
  (put-string stderr "Error: `")
  (put-string stderr name)
  (put-string stderr "` ")
  (put-string stderr what)
  (put-string stderr " before its definition\n")
  (exit-process (raw 1)))
