;; The platform layer, x86-64 Linux: the only operations the runtime asks of the OS, and the only
;; file using syscall. Another OS or architecture reimplements these definitions (and _start,
;; which passes the initial stack to init-runtime).
;; A failing operation returns minus an error code, which os-error-message describes.
;; Paths and environment entries are C strings.

;; The initial stack: argc, then the arguments, C strings
(define-data arguments 1)

;; The environment variables, NAME=VALUE C strings ending with 0
(define-data environment 1)

;; The error code of a file name the runtime cannot pass to the OS (too long, or holding a NUL)
(define-constant name-too-long-error (raw 36)) ; ENAMETOOLONG

;; Called by init-runtime with the initial stack: argc, the arguments, 0, then the environment
(define (init-platform (raw sp))
  (declare unsafe noalloc (result void))
  (store arguments (raw 0) sp)
  (store environment (raw 0) (raw+ sp (raw<< (raw+ (load sp (raw 0)) (raw 2)) (raw 3)))))

(define (os-argument-count)
  (declare unsafe noalloc (result raw))
  (load (load arguments (raw 0)) (raw 0)))

;; Argument k, a C string
(define (os-argument (raw k))
  (declare unsafe inline (result raw))
  (load (load arguments (raw 0)) (raw+ k (raw 1))))

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

;; Reads at most n bytes from fd to buf: the bytes read, 0 at the end of the file
(define (os-read (raw fd) (raw buf) (raw n))
  (declare unsafe noalloc (result raw))
  (syscall 0 fd buf n))

;; Writes at most n bytes from buf to fd: the bytes written
(define (os-write (raw fd) (raw buf) (raw n))
  (declare unsafe noalloc (result raw))
  (syscall 1 fd buf n))

;; struct pollfd {int fd; short events; short revents;} for os-input-ready?
(define-data poll-fd 1)

;; Whether fd may be read without blocking (also at the end of the file)
(define (os-input-ready? (raw fd))
  (declare unsafe noalloc)
  (store poll-fd (raw 0) (bit-or fd (raw<< (raw 1) (raw 32)))) ; POLLIN
  (raw> (syscall 7 poll-fd (raw 1) (raw 0)) (raw 0)))

;; Opens the file path for reading: a descriptor
(define (os-open-input (raw path))
  (declare unsafe noalloc (result raw))
  (syscall 2 path (raw 0) (raw 0))) ; O_RDONLY

;; Opens the file path for writing, created or truncated: a descriptor
(define (os-open-output (raw path))
  (declare unsafe noalloc (result raw))
  (syscall 2 path (raw 577) (raw 438))) ; O_WRONLY | O_CREAT | O_TRUNC, 0666

(define (os-close (raw fd))
  (declare unsafe noalloc (result void))
  (syscall 3 fd)
  undef)

(define (os-file-exists? (raw path))
  (declare unsafe noalloc)
  (raw= (syscall 21 path (raw 0)) (raw 0))) ; access, F_OK

;; Whether the file path was deleted
(define (os-delete-file (raw path))
  (declare unsafe noalloc)
  (raw= (syscall 87 path) (raw 0))) ; unlink

;; Sets the permissions of the file path to the mode bits: 0
(define (os-chmod (raw path) (raw mode))
  (declare unsafe noalloc (result raw))
  (syscall 90 path mode))

;; n zeroed bytes, touched lazily, or 0 if the OS has no room
(define (os-map (raw n))
  (declare unsafe noalloc (result raw))
  ;; mmap(0, n, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0)
  (let ((p (syscall 9 (raw 0) n (raw 3) (raw 34) (raw -1) (raw 0))))
    (if (raw>u p (raw -4096)) (raw 0) p)))

;; n bytes like os-map, but not counted against the commit limit (MAP_NORESERVE), for a region
;; touched only in part
(define (os-reserve (raw n))
  (declare unsafe noalloc (result raw))
  (let ((p (syscall 9 (raw 0) n (raw 3) (raw 16418) (raw -1) (raw 0))))
    (if (raw>u p (raw -4096)) (raw 0) p)))

;; Releases the n bytes at p, from os-map
(define (os-unmap (raw p) (raw n))
  (declare unsafe noalloc (result void))
  (syscall 11 p n)
  undef)

(define (os-exit (raw status))
  (declare unsafe noalloc (result never))
  (syscall 231 status)) ; exit_group

;; The arguments of execveat(2): the program's name, then 0, which is also the empty path
(define-data execute-arguments 2)

;; Replaces the process with the executable of the n bytes at buf, named name in its arguments:
;; its bytes in a memfd, run by execveat(2); returns only on failure.
;; The memfd is named marelle, since memfd_create(2) rejects names over 249 bytes.
(define (os-execute (raw buf) (raw n) (raw name))
  (declare unsafe noalloc (result raw))
  (let ((fd (syscall 319 "marelle" (raw 1)))) ; MFD_CLOEXEC
    (if (raw< fd (raw 0))
        fd
        (begin
          (write-all fd buf n)
          (store execute-arguments (raw 0) name)
          (store execute-arguments (raw 1) (raw 0))
          (syscall 322 fd (raw+ execute-arguments (raw 8)) execute-arguments
                   (load environment (raw 0)) (raw 4096)))))) ; AT_EMPTY_PATH

;; The text of the error code, a fixnum
(define (os-error-message code)
  (case code
    ((2) "no such file or directory")
    ((13) "permission denied")
    ((17) "file exists")
    ((20) "not a directory")
    ((21) "is a directory")
    ((24) "too many open files")
    ((36) "file name too long")
    (else (format "error ~a" code))))
