;; Ports: [port-type, flags, fd, position, end, buffer], all raw but the buffer, last so the
;; collector scans only it; a descriptor port's buffer is a bytevector, holding position bytes
;; for an output port, its unread bytes [position, end) for an input port. A string output
;; port's is a bytevector of position UTF-8 bytes, doubled when full; a string input port's is
;; the string, read from char position to end.

(define-constant input-flag (raw 1))
(define-constant output-flag (raw 2))
(define-constant string-flag (raw 4))
(define-constant closed-flag (raw 8))

;; The bytes of a descriptor port's buffer
(define-constant port-buffer-size (raw 8192))

;; The console ports and their buffers, static, set up by init-console-ports!
(define-data stdin 6)
(define-data stdout 6)
(define-data stderr 6)
(define-data stdin-buffer 1026)
(define-data stdout-buffer 1026)
(define-data stderr-buffer 1026)

;; A string output port without a buffer, which only counts the bytes written to it
(define-data counting-port 6)

;; The current input, output and error ports, and the list of the open descriptor output ports
;; but the console ones, flushed at exit: roots of the collector
(define-data current-ports 3)
(define-data output-ports 1)

(define (port-flags (raw p))
  (declare unsafe inline (result raw))
  (load p (raw 1)))

(define (port-fd (raw p))
  (declare unsafe inline (result raw))
  (load p (raw 2)))

(define (port-position (raw p))
  (declare unsafe inline (result raw))
  (load p (raw 3)))

(define (set-port-position! (raw p) (raw n))
  (declare unsafe inline (result void))
  (store p (raw 3) n))

(define (port-end (raw p))
  (declare unsafe inline (result raw))
  (load p (raw 4)))

(define (set-port-end! (raw p) (raw n))
  (declare unsafe inline (result void))
  (store p (raw 4) n))

;; The address of the buffer's first byte
(define (port-bytes (raw p))
  (declare unsafe inline (result raw))
  (raw+ (load p (raw 5)) (raw 16)))

(define (port-capacity (raw p))
  (declare unsafe inline (result raw))
  (load (load p (raw 5)) (raw 1)))

(define (init-port! (raw p) (raw flags) (raw fd) (raw buffer))
  (declare unsafe noalloc (result void))
  (store buffer (raw 0) bytevector-type)
  (store buffer (raw 1) port-buffer-size)
  (store p (raw 0) port-type)
  (store p (raw 1) flags)
  (store p (raw 2) fd)
  (store p (raw 5) buffer))

(define (init-console-ports!)
  (declare unsafe noalloc (result void))
  (init-port! stdin input-flag (raw 0) stdin-buffer)
  (init-port! stdout output-flag (raw 1) stdout-buffer)
  (init-port! stderr output-flag (raw 2) stderr-buffer)
  (store current-ports (raw 0) stdin)
  (store current-ports (raw 1) stdout)
  (store counting-port (raw 0) port-type)
  (store counting-port (raw 1) (bit-or output-flag string-flag))
  (store current-ports (raw 2) stderr)
  (store output-ports (raw 0) '()))

(define (port? x)
  (declare unsafe inline public)
  (and (tag=? x (raw 0))
       (not (raw= (as-raw x) (raw 0)))
       (raw= (load (as-raw x) (raw 0)) port-type)))

(define (port-flag? port (raw flag))
  (declare unsafe inline)
  (not (raw= (bit-and (port-flags (as-raw port)) flag) (raw 0))))

(define (input-port? x)
  (declare unsafe inline public)
  (and (port? x) (port-flag? x input-flag)))

(define (output-port? x)
  (declare unsafe inline public)
  (and (port? x) (port-flag? x output-flag)))

(define (textual-port? x)
  (declare inline public)
  (port? x))

;; port is not an open port of the direction
(define (port-error who port output?)
  (declare unsafe (result never))
  (if (and (port? port) (port-flag? port (if output? output-flag input-flag)))
      (argument-error who 'closed-port port)
      (argument-error who (if output? 'textual-output-port 'textual-input-port) port)))

;; Checks that port is an open port of the direction flag
(define (check-port port (raw flag) who)
  (declare unsafe inline (result void))
  (unless (and (port? port)
               (raw= (bit-and (port-flags (as-raw port)) (bit-or flag closed-flag)) flag))
    (port-error who port (raw= flag output-flag))))

;; The EOF object, the immediate of payload 6
(define-constant eof-word (raw 50))

(define (eof-object)
  (declare unsafe inline public)
  (as-val eof-word))

(define (eof-object? x)
  (declare unsafe inline public)
  (raw= (as-raw x) eof-word))

;; Current port i with no argument, else set to the port, of the direction flag
(define (current-port args (raw i) (raw flag) who)
  (declare unsafe inline)
  (cond
   ((null? args) (as-val (load current-ports i)))
   ((and (null? (cdr args)) (port? (car args)) (port-flag? (car args) flag))
    (store current-ports i (car args))
    undef)
   ((null? (cdr args))
    (argument-error who (if (raw= flag output-flag) 'textual-output-port 'textual-input-port)
                    (car args)))
   (else (argument-count-error who (length args)))))

(define (current-input-port)
  (declare unsafe inline public)
  (as-val (load current-ports (raw 0))))

(define (variadic-current-input-port . port)
  (declare unsafe)
  (current-port port (raw 0) input-flag current-input-port))

(define (current-output-port)
  (declare unsafe inline public)
  (as-val (load current-ports (raw 1))))

(define (variadic-current-output-port . port)
  (declare unsafe)
  (current-port port (raw 1) output-flag current-output-port))

(define (current-error-port)
  (declare unsafe inline public)
  (as-val (load current-ports (raw 2))))

(define (variadic-current-error-port . port)
  (declare unsafe)
  (current-port port (raw 2) output-flag current-error-port))

;; The port of the optional arguments rest, after n others, else current port i
(define (optional-port rest (raw i) who n)
  (declare unsafe inline)
  (cond
   ((null? rest) (as-val (load current-ports i)))
   ((null? (cdr rest)) (car rest))
   (else (argument-count-error who (+ n (length rest))))))

;; Writes x to the open output port, like write when write? is 1, else like display
(define (port-put! port x (raw write?))
  (declare unsafe inline)
  (if (port-flag? port string-flag)
      (string-port-put! port x (raw= write? (raw 1)))
      (begin
        (put-datum (as-raw port) write? x)
        undef)))

;; The bytes of x as written by put-datum
(define (datum-bytes (raw write?) x)
  (declare unsafe noalloc (result raw))
  (set-port-position! counting-port (raw 0))
  (put-datum counting-port write? x)
  (port-position counting-port))

;; Measures x, makes room for it, then writes it
(define (string-port-put! port x write?)
  (declare unsafe)
  (reserve! port (make-fixnum (datum-bytes (if write? (raw 1) (raw 0)) x)))
  (put-datum (as-raw port) (if write? (raw 1) (raw 0)) x)
  undef)

;; Makes room for n more bytes in the string output port, doubling its buffer
(define (reserve! port n)
  (declare unsafe)
  (let ((need (+ (make-fixnum (port-position (as-raw port))) n))
        (capacity (make-fixnum (port-capacity (as-raw port)))))
    (when (> need capacity)
      (let ((new (alloc-bytevector (max need (* 2 capacity)))))
        (copy-words (raw+ (as-raw new) (raw 16)) (port-bytes (as-raw port))
                    (raw>>u (raw+ (port-position (as-raw port)) (raw 7)) (raw 3)))
        (store (as-raw port) (raw 5) new)))
    undef))

(define (display x port)
  (declare unsafe public)
  (check-port port output-flag display)
  (port-put! port x (raw 0))
  undef)

(define (variadic-display x . port)
  (declare unsafe)
  (display x (optional-port port (raw 1) display 1)))

(define (write x port)
  (declare unsafe public)
  (check-port port output-flag write)
  (port-put! port x (raw 1))
  undef)

(define (variadic-write x . port)
  (declare unsafe)
  (write x (optional-port port (raw 1) write 1)))

(define (newline port)
  (declare unsafe public)
  (check-port port output-flag newline)
  (port-put! port #\newline (raw 0))
  undef)

(define (variadic-newline . port)
  (declare unsafe)
  (newline (optional-port port (raw 1) newline 0)))

(define (write-char c port)
  (declare unsafe public)
  (unless (char? c)
    (argument-error write-char 'char c))
  (check-port port output-flag write-char)
  (port-put! port c (raw 0))
  undef)

(define (variadic-write-char c . port)
  (declare unsafe)
  (write-char c (optional-port port (raw 1) write-char 1)))

(define (write-string s port)
  (declare unsafe public)
  (check-string s write-string)
  (check-port port output-flag write-string)
  (port-put! port s (raw 0))
  undef)

;; (write-string s [port [start [end]]])
(define (variadic-write-string s . rest)
  (check-string s write-string)
  (cond
   ((null? rest) (write-string s (current-output-port)))
   ((null? (cdr rest)) (write-string s (car rest)))
   ((null? (cddr rest)) (write-string (substring s (cadr rest) (string-length s)) (car rest)))
   ((null? (cdddr rest)) (write-string (substring s (cadr rest) (caddr rest)) (car rest)))
   (else (argument-count-error write-string (+ 1 (length rest))))))

(define (flush-output-port port)
  (declare unsafe public)
  (check-port port output-flag flush-output-port)
  (unless (port-flag? port string-flag)
    (flush-port (as-raw port)))
  undef)

(define (variadic-flush-output-port . port)
  (declare unsafe)
  (flush-output-port (optional-port port (raw 1) flush-output-port 0)))

;; Moves the unread bytes of the descriptor input port to the front of its buffer and reads more
;; after them, the console flushed first so that a prompt shows; the bytes read, 0 at the end
(define (fill-port! (raw port))
  (declare unsafe noalloc (result raw))
  (flush-port stdout)
  (flush-port stderr)
  (let ((left (raw- (port-end port) (port-position port))))
    (copy-bytes! (port-bytes port) (raw+ (port-bytes port) (port-position port)) left)
    (set-port-position! port (raw 0))
    (set-port-end! port left)
    (let ((n (syscall 0 (port-fd port) (raw+ (port-bytes port) left) (raw- port-buffer-size left))))
      (if (raw> n (raw 0))
          (begin
            (set-port-end! port (raw+ left n))
            n)
          (raw 0)))))

;; Whether the descriptor input port holds n unread bytes, read until it does or the file ends
(define (port-holds? (raw port) (raw n))
  (declare unsafe noalloc)
  (let fill-next ()
    (cond
     ((raw>=u (raw- (port-end port) (port-position port)) n) #t)
     ((raw= (fill-port! port) (raw 0)) #f)
     (else (fill-next)))))

;; The length of the UTF-8 sequence the byte b leads, or 0 if it leads none
(define (utf8-length (raw b))
  (declare unsafe inline (result raw))
  (cond
   ((raw<u b (raw 128)) (raw 1))
   ((raw<u b (raw 194)) (raw 0)) ; a continuation byte, or overlong
   ((raw<u b (raw 224)) (raw 2))
   ((raw<u b (raw 240)) (raw 3))
   ((raw<u b (raw 245)) (raw 4))
   (else (raw 0))))

;; The code of the len-byte UTF-8 sequence at p, or -1 if it has a bad continuation byte, is
;; overlong, a surrogate or past U+10FFFF
(define (utf8-code (raw p) (raw len))
  (declare unsafe noalloc (result raw))
  (let byte-next ((i (raw 1)))
    (cond
     ((raw= i len)
      ;; The lead byte's payload: 255 >> (len + 1) is 31, 15 or 7
      (let ((n (continue-utf8 p (raw 1) len
                              (bit-and (load8 p (raw 0)) (raw>>u (raw 255) (raw+ len (raw 1)))))))
        (if (or (and (raw= len (raw 3))
                     (or (raw<u n (raw 2048)) (and (raw>= n (raw 55296)) (raw<= n (raw 57343)))))
                (and (raw= len (raw 4)) (or (raw<u n (raw 65536)) (raw>u n (raw 1114111)))))
            (raw -1)
            n)))
     ((raw= (bit-and (load8 p i) (raw 192)) (raw 128)) (byte-next (raw+ i (raw 1))))
     (else (raw -1)))))

;; The next char of the descriptor input port, or the EOF object, consumed unless peek? is 1;
;; an invalid or truncated sequence is U+FFFD for its first byte, as in Chez
(define (read-fd-char (raw port) (raw peek?))
  (declare unsafe noalloc)
  (if (port-holds? port (raw 1))
      (let* ((len (utf8-length (load8 (port-bytes port) (port-position port))))
             (n (cond
                 ((raw= len (raw 1)) (load8 (port-bytes port) (port-position port)))
                 ((and (raw>u len (raw 1)) (port-holds? port len))
                  (utf8-code (raw+ (port-bytes port) (port-position port)) len))
                 (else (raw -1)))))
        (when (raw= peek? (raw 0))
          (set-port-position! port (raw+ (port-position port) (if (raw= n (raw -1)) (raw 1) len))))
        (code->char (if (raw= n (raw -1)) (raw 65533) n)))
      (eof-object)))

;; The next char of the string input port, or the EOF object, consumed unless peek? is 1
(define (read-string-char (raw port) (raw peek?))
  (declare unsafe noalloc)
  (let ((i (port-position port)))
    (if (raw= i (port-end port))
        (eof-object)
        (begin
          (when (raw= peek? (raw 0))
            (set-port-position! port (raw+ i (raw 1))))
          (code->char (string-code (as-val (load port (raw 5))) i))))))

(define (port-read-char port (raw peek?))
  (declare unsafe inline)
  (if (port-flag? port string-flag)
      (read-string-char (as-raw port) peek?)
      (read-fd-char (as-raw port) peek?)))

(define (read-char port)
  (declare unsafe public)
  (check-port port input-flag read-char)
  (port-read-char port (raw 0)))

(define (variadic-read-char . port)
  (declare unsafe)
  (read-char (optional-port port (raw 0) read-char 0)))

(define (peek-char port)
  (declare unsafe public)
  (check-port port input-flag peek-char)
  (port-read-char port (raw 1)))

(define (variadic-peek-char . port)
  (declare unsafe)
  (peek-char (optional-port port (raw 0) peek-char 0)))

;; The chars up to the next newline, which is consumed, or to the end; the EOF object at the end
(define (read-line port)
  (declare unsafe public)
  (check-port port input-flag read-line)
  (let ((c (port-read-char port (raw 0))))
    (if (eof-object? c)
        c
        (let char-next ((c c)
                        (chars '()))
          (if (or (eof-object? c) (eq? c #\newline))
              (list->string (reverse chars))
              (char-next (port-read-char port (raw 0)) (cons c chars)))))))

(define (variadic-read-line . port)
  (declare unsafe)
  (read-line (optional-port port (raw 0) read-line 0)))

;; The next k chars, fewer at the end; the EOF object at the end
(define (read-string k port)
  (declare unsafe public)
  (check-count k read-string 'nonnegative-fixnum)
  (check-port port input-flag read-string)
  (let char-next ((k k)
                  (chars '()))
    (if (eq? k 0)
        (list->string (reverse chars))
        (let ((c (port-read-char port (raw 0))))
          (cond
           ((not (eof-object? c)) (char-next (- k 1) (cons c chars)))
           ((null? chars) c)
           (else (list->string (reverse chars))))))))

(define (variadic-read-string k . port)
  (declare unsafe)
  (read-string k (optional-port port (raw 0) read-string 1)))

;; struct pollfd {int fd; short events; short revents;} for char-ready?
(define-data poll-fd 1)

;; Whether a char may be read without blocking: a string port, buffered bytes, or poll(2) says
;; so (also at the end of the file)
(define (port-ready? port)
  (declare unsafe noalloc)
  (let ((p (as-raw port)))
    (or (not (raw= (bit-and (port-flags p) string-flag) (raw 0)))
        (raw<u (port-position p) (port-end p))
        (begin
          (store poll-fd (raw 0) (bit-or (port-fd p) (raw<< (raw 1) (raw 32)))) ; POLLIN
          (raw> (syscall 7 poll-fd (raw 1) (raw 0)) (raw 0))))))

(define (char-ready? port)
  (declare unsafe public)
  (check-port port input-flag char-ready?)
  (port-ready? port))

(define (variadic-char-ready? . port)
  (declare unsafe)
  (char-ready? (optional-port port (raw 0) char-ready? 0)))

;; Files

;; The longest file name, its 0 included
(define-constant path-max (raw 4096))
(define-data path-buffer 512)

;; The file name s as a C string in path-buffer, or 0 if it is too long or holds a NUL
(define (file-name s)
  (declare unsafe noalloc (result raw))
  (let char-next ((i (raw 0))
                  (j (raw 0)))
    (cond
     ((raw= i (string-nchars s))
      (store8 path-buffer j (raw 0))
      path-buffer)
     ((or (raw= (string-code s i) (raw 0)) (raw>u j (raw- path-max (raw 5)))) (raw 0))
     (else
      (char-next (raw+ i (raw 1)) (raw+ j (store-utf8! (raw+ path-buffer j) (string-code s i))))))))

;; The file name could not be opened, open(2) failing with errno
(define (open-error who name errno)
  (declare (result never))
  (library-errorf who "failed for ~a: ~a"
                  (list name
                        (case errno
                          ((2) "no such file or directory")
                          ((13) "permission denied")
                          ((17) "file exists")
                          ((20) "not a directory")
                          ((21) "is a directory")
                          ((24) "too many open files")
                          ((36) "file name too long")
                          (else (format "error ~a" errno))))))

;; A descriptor port of the direction flag for the file name, opened with the open(2) flags;
;; a name file-name rejects fails like a long one
(define (open-file name (raw open-flags) (raw flag) who)
  (declare unsafe inline)
  (check-string name who)
  (let ((path (file-name name)))
    (let ((fd (make-fixnum (if (raw= path (raw 0)) (raw -36) (syscall 2 path open-flags (raw 438)))))) ; 0666
      (when (< fd 0)
        (open-error who name (- 0 fd)))
      (make-fd-port fd flag))))

;; A port of the fixnum flags over the buffer, its fd, position and end 0
(define (make-port buffer flags)
  (declare unsafe inline)
  (let ((p (alloc (raw 6))))
    (store p (raw 0) port-type)
    (store p (raw 1) (fixnum-value flags))
    (store p (raw 5) buffer)
    (as-val p)))

(define (make-fd-port fd (raw flag))
  (declare unsafe inline)
  (let ((p (make-port (alloc-bytevector (make-fixnum port-buffer-size)) (make-fixnum flag))))
    (store (as-raw p) (raw 2) (fixnum-value fd))
    p))

;; The name as a C string, or 0 after checking it is a string
(define (checked-file-name name who)
  (declare unsafe inline (result raw))
  (check-string name who)
  (file-name name))

;; access(2) with F_OK
(define (file-exists? name)
  (declare unsafe public)
  (let ((path (checked-file-name name file-exists?)))
    (and (not (raw= path (raw 0)))
         (raw= (syscall 21 path (raw 0)) (raw 0)))))

;; unlink(2); #f when it fails, as in Chez
(define (delete-file name)
  (declare unsafe public)
  (let ((path (checked-file-name name delete-file)))
    (and (not (raw= path (raw 0)))
         (raw= (syscall 87 path) (raw 0)))))

(define (open-input-file name)
  (declare unsafe public)
  (open-file name (raw 0) input-flag open-input-file)) ; O_RDONLY

(define (open-output-file name)
  (declare unsafe public)
  (let ((port (open-file name (raw 577) output-flag open-output-file))) ; O_WRONLY | O_CREAT | O_TRUNC
    (store output-ports (raw 0) (cons port (as-val (load output-ports (raw 0)))))
    port))

;; Flushes a descriptor output port; a console port keeps its descriptor, for the runtime's
;; messages
(define (close-port! port)
  (declare unsafe)
  (when (and (port-flag? port output-flag) (not (port-flag? port string-flag)))
    (flush-port (as-raw port))
    (store output-ports (raw 0) (remq port (as-val (load output-ports (raw 0))))))
  (when (raw>u (port-fd (as-raw port)) (raw 2))
    (syscall 3 (port-fd (as-raw port))))
  (store (as-raw port) (raw 1) (bit-or (port-flags (as-raw port)) closed-flag))
  undef)

(define (close-port port)
  (declare unsafe public)
  (unless (port? port)
    (argument-error close-port 'port port))
  (unless (port-flag? port closed-flag)
    (close-port! port))
  undef)

(define (close-input-port port)
  (declare unsafe public)
  (unless (input-port? port)
    (argument-error close-input-port 'input-port port))
  (close-port port))

(define (close-output-port port)
  (declare unsafe public)
  (unless (output-port? port)
    (argument-error close-output-port 'output-port port))
  (close-port port))

(define (input-port-open? port)
  (declare unsafe public)
  (unless (port? port)
    (argument-error input-port-open? 'port port))
  (and (port-flag? port input-flag) (not (port-flag? port closed-flag))))

(define (output-port-open? port)
  (declare unsafe public)
  (unless (port? port)
    (argument-error output-port-open? 'port port))
  (and (port-flag? port output-flag) (not (port-flag? port closed-flag))))

;; proc's value, after closing the port
(define (call-with-port port proc)
  (declare unsafe public)
  (unless (port? port)
    (argument-error call-with-port 'port port))
  (let ((result (proc port)))
    (close-port port)
    result))

(define (call-with-input-file name proc)
  (declare public)
  (call-with-port (open-input-file name) proc))

(define (call-with-output-file name proc)
  (declare public)
  (call-with-port (open-output-file name) proc))

(define (with-input-from-file name thunk)
  (declare public)
  (let* ((port (open-input-file name))
         (result (parameterize ((current-input-port port))
                   (thunk))))
    (close-port port)
    result))

(define (with-output-to-file name thunk)
  (declare public)
  (let* ((port (open-output-file name))
         (result (parameterize ((current-output-port port))
                   (thunk))))
    (close-port port)
    result))

;; String ports

(define (open-input-string s)
  (declare unsafe public)
  (check-string s open-input-string)
  (let ((p (make-port s (make-fixnum (bit-or input-flag string-flag)))))
    (set-port-end! (as-raw p) (string-nchars s))
    p))

(define (open-output-string)
  (declare unsafe public)
  (make-port (alloc-bytevector 64) (make-fixnum (bit-or output-flag string-flag))))

;; The chars written so far; the port is not reset (it is in Chez)
(define (get-output-string port)
  (declare unsafe public)
  (unless (and (output-port? port) (port-flag? port string-flag))
    (argument-error get-output-string 'string-output-port port))
  (utf8-range->string (as-val (load (as-raw port) (raw 5))) 0 (make-fixnum (port-position (as-raw port)))))

;; What thunk writes to the current output port, as a string (Chez)
(define (with-output-to-string thunk)
  (declare public)
  (let ((port (open-output-string)))
    (parameterize ((current-output-port port))
      (thunk))
    (get-output-string port)))

;; thunk's value, reading from the string s as the current input port (Chez)
(define (with-input-from-string s thunk)
  (declare public)
  (parameterize ((current-input-port (open-input-string s)))
    (thunk)))
