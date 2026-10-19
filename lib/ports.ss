;; Ports: [port-type, flags, fd, position, end, buffer], all raw but the buffer, last so the
;; collector scans only it; a descriptor port's buffer is a bytevector, holding position bytes
;; for an output port, its unread bytes [position, end) for an input port

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

;; The current input, output and error ports, roots of the collector
(define-data current-ports 3)

;; Bytevectors, private for now: [bytevector-type, nbytes, bytes...], all raw
(define (bytevector-words (raw n))
  (declare unsafe inline (result raw))
  (raw+ (raw 2) (raw>>u (raw+ n (raw 7)) (raw 3))))

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
  (store current-ports (raw 2) stderr))

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

;; Checks that port is an open port of the direction flag
(define (check-port port (raw flag) (raw msg))
  (declare unsafe inline (result void))
  (unless (and (port? port)
               (raw= (bit-and (port-flags (as-raw port)) (bit-or flag closed-flag)) flag))
    (scheme-error msg port)))

;; The EOF object, the immediate of payload 6
(define-constant eof-word (raw 50))

(define (eof-object)
  (declare unsafe inline public)
  (as-val eof-word))

(define (eof-object? x)
  (declare unsafe inline public)
  (raw= (as-raw x) eof-word))

;; Current port i with no argument, else set to the port, of the direction flag
(define (current-port args (raw i) (raw flag) (raw msg))
  (declare unsafe inline)
  (cond
   ((null? args) (as-val (load current-ports i)))
   ((and (null? (cdr args)) (port? (car args)) (port-flag? (car args) flag))
    (store current-ports i (car args))
    undef)
   (else (scheme-error msg args))))

(define (current-input-port)
  (declare unsafe inline public)
  (as-val (load current-ports (raw 0))))

(define (variadic-current-input-port . port)
  (declare unsafe)
  (current-port port (raw 0) input-flag "`current-input-port` expected an input port"))

(define (current-output-port)
  (declare unsafe inline public)
  (as-val (load current-ports (raw 1))))

(define (variadic-current-output-port . port)
  (declare unsafe)
  (current-port port (raw 1) output-flag "`current-output-port` expected an output port"))

(define (current-error-port)
  (declare unsafe inline public)
  (as-val (load current-ports (raw 2))))

(define (variadic-current-error-port . port)
  (declare unsafe)
  (current-port port (raw 2) output-flag "`current-error-port` expected an output port"))

;; The port of the optional arguments rest, else current port i
(define (optional-port rest (raw i) (raw msg))
  (declare unsafe inline)
  (cond
   ((null? rest) (as-val (load current-ports i)))
   ((null? (cdr rest)) (car rest))
   (else (scheme-error msg rest))))

;; Writes x to the open output port, like write when write? is 1, else like display
(define (port-put! port x (raw write?))
  (declare unsafe inline (result void))
  (put-datum (as-raw port) write? x))

(define (display x port)
  (declare unsafe public)
  (check-port port output-flag "`display` expected an open output port")
  (port-put! port x (raw 0))
  undef)

(define (variadic-display x . port)
  (declare unsafe)
  (display x (optional-port port (raw 1) "`display` expected an optional port")))

(define (write x port)
  (declare unsafe public)
  (check-port port output-flag "`write` expected an open output port")
  (port-put! port x (raw 1))
  undef)

(define (variadic-write x . port)
  (declare unsafe)
  (write x (optional-port port (raw 1) "`write` expected an optional port")))

(define (newline port)
  (declare unsafe public)
  (check-port port output-flag "`newline` expected an open output port")
  (port-put! port #\newline (raw 0))
  undef)

(define (variadic-newline . port)
  (declare unsafe)
  (newline (optional-port port (raw 1) "`newline` expected an optional port")))

(define (write-char c port)
  (declare unsafe public)
  (unless (char? c)
    (scheme-error "`write-char` expected a character" c))
  (check-port port output-flag "`write-char` expected an open output port")
  (port-put! port c (raw 0))
  undef)

(define (variadic-write-char c . port)
  (declare unsafe)
  (write-char c (optional-port port (raw 1) "`write-char` expected an optional port")))

(define (write-string s port)
  (declare unsafe public)
  (check-string s "`write-string` expected a string")
  (check-port port output-flag "`write-string` expected an open output port")
  (port-put! port s (raw 0))
  undef)

;; (write-string s [port [start [end]]])
(define (variadic-write-string s . rest)
  (declare unsafe)
  (check-string s "`write-string` expected a string")
  (cond
   ((null? rest) (write-string s (current-output-port)))
   ((null? (cdr rest)) (write-string s (car rest)))
   ((null? (cddr rest)) (write-string (substring s (cadr rest) (string-length s)) (car rest)))
   ((null? (cdddr rest)) (write-string (substring s (cadr rest) (caddr rest)) (car rest)))
   (else (scheme-error "`write-string` expected at most 4 arguments" rest))))

(define (flush-output-port port)
  (declare unsafe public)
  (check-port port output-flag "`flush-output-port` expected an open output port")
  (flush-port (as-raw port))
  undef)

(define (variadic-flush-output-port . port)
  (declare unsafe)
  (flush-output-port (optional-port port (raw 1) "`flush-output-port` expected an optional port")))

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

(define (port-read-char port (raw peek?))
  (declare unsafe inline)
  (read-fd-char (as-raw port) peek?))

(define (read-char port)
  (declare unsafe public)
  (check-port port input-flag "`read-char` expected an open input port")
  (port-read-char port (raw 0)))

(define (variadic-read-char . port)
  (declare unsafe)
  (read-char (optional-port port (raw 0) "`read-char` expected an optional port")))

(define (peek-char port)
  (declare unsafe public)
  (check-port port input-flag "`peek-char` expected an open input port")
  (port-read-char port (raw 1)))

(define (variadic-peek-char . port)
  (declare unsafe)
  (peek-char (optional-port port (raw 0) "`peek-char` expected an optional port")))

;; The chars up to the next newline, which is consumed, or to the end; the EOF object at the end
(define (read-line port)
  (declare unsafe public)
  (check-port port input-flag "`read-line` expected an open input port")
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
  (read-line (optional-port port (raw 0) "`read-line` expected an optional port")))

;; The next k chars, fewer at the end; the EOF object at the end
(define (read-string k port)
  (declare unsafe public)
  (check-count k "`read-string` expected a nonnegative fixnum")
  (check-port port input-flag "`read-string` expected an open input port")
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
  (read-string k (optional-port port (raw 0) "`read-string` expected an optional port")))

;; struct pollfd {int fd; short events; short revents;} for char-ready?
(define-data poll-fd 1)

;; Whether a char may be read without blocking: buffered bytes, or poll(2) says so (also at the
;; end of the file)
(define (port-ready? port)
  (declare unsafe noalloc)
  (let ((p (as-raw port)))
    (or (raw<u (port-position p) (port-end p))
        (begin
          (store poll-fd (raw 0) (bit-or (port-fd p) (raw<< (raw 1) (raw 32)))) ; POLLIN
          (raw> (syscall 7 poll-fd (raw 1) (raw 0)) (raw 0))))))

(define (char-ready? port)
  (declare unsafe public)
  (check-port port input-flag "`char-ready?` expected an open input port")
  (port-ready? port))

(define (variadic-char-ready? . port)
  (declare unsafe)
  (char-ready? (optional-port port (raw 0) "`char-ready?` expected an optional port")))
