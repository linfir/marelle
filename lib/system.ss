;; The environment variables, NAME=VALUE C strings ending with 0
(define-data environment 1)

(define-constant stdout (raw 1))
(define-constant stderr (raw 2))

;; Writes the n bytes at buf to the file descriptor fd, stopping on an error
(define (write-bytes (raw fd) (raw buf) (raw n))
  (declare unsafe noalloc (result void))
  (let write-next ((done (raw 0)))
    (if (raw<u done n)
        (let ((written (syscall 1 fd (raw+ buf done) (raw- n done))))
          (if (raw< written (raw 0))
              undef
              (write-next (raw+ done written))))
        undef)))

(define (c-string-length (raw s))
  (declare unsafe noalloc (result raw))
  (let length-next ((i (raw 0)))
    (if (raw= (load8 s i) (raw 0))
        i
        (length-next (raw+ i (raw 1))))))

(define (put-string (raw fd) (raw s))
  (declare unsafe noalloc (result void))
  (write-bytes fd s (c-string-length s)))

;; Prints the statistics on stderr when SCHEME_GC_STATS is set, then exits
(define (exit-process (raw status))
  (declare unsafe noalloc (result never))
  (unless (raw= (load gc-stats (raw 0)) (raw 0))
    (print-gc-stats))
  (syscall 231 status)) ; exit_group

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

;; Called from _start with the value of scheme_main
(define (finish-program v)
  (declare unsafe noalloc (result never))
  (display stdout v)
  (exit-process (raw 0)))

;; Prints "Error: msg: x" and exits
(define (scheme-error (raw msg) x)
  (declare unsafe noalloc (result never))
  (put-string stderr "Error: ")
  (put-string stderr msg)
  (put-string stderr ": ")
  (display stderr x)
  (put-string stderr "\n")
  (exit-process (raw 1)))

;; A letrec variable used or assigned before its init
(define (definition-error (raw name) (raw what))
  (declare unsafe noalloc (result never))
  (put-string stderr "Error: `")
  (put-string stderr name)
  (put-string stderr "` ")
  (put-string stderr what)
  (put-string stderr " before its definition\n")
  (exit-process (raw 1)))
