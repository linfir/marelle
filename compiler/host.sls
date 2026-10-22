;; Under Chez, what stage1 has and Chez lacks:
;; the files and datums schemec --embed and --embed-datum embedded, none (embedded-file and
;; embedded-datum of lib/system.ss), execute-bytevector (lib/ports.ss), R7RS's bytevector-copy!
;; (lib/bytevectors.ss) and get-environment-variable (lib/ports.ss);
;; not flattened

(library (host)
  (export embedded-file embedded-datum execute-bytevector bytevector-copy! get-environment-variable)
  (import (rename (chezscheme) (bytevector-copy! chez-bytevector-copy!)))


  (define (embedded-file name)
    #f)


  (define (embedded-datum name)
    #f)


  ;; Runs the executable bv from a temporary file, then exits with its status,
  ;; 128 + n when signal n killed it, as a shell reports it;
  ;; Chez has no execve, so the program's name is the file's, not name
  (define (execute-bytevector bv name)
    (let* ((ports (process "mktemp"))
           (path (get-line (car ports))))
      (close-port (car ports))
      (close-port (cadr ports))
      (delete-file path)
      (let ((port (open-file-output-port path)))
        (put-bytevector port bv)
        (close-port port))
      (chmod path #o700)
      (flush-output-port (current-output-port))
      (flush-output-port (current-error-port))
      (let ((status (system path)))
        (delete-file path)
        (exit (if (< status 0) (- 128 status) status)))))


  ;; The bytes [start, end) of from to to at at, in R7RS's order, not R6RS's
  (define (bytevector-copy! to at from start end)
    (chez-bytevector-copy! from start to at (- end start)))


  (define (get-environment-variable name)
    (getenv name)))
