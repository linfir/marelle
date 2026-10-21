;; Under Chez, what stage1 has and Chez lacks:
;; the files schemec --embed embedded, none (embedded-file of lib/system.ss),
;; execute-bytevector (lib/ports.ss) and R7RS's bytevector-copy! (lib/bytevectors.ss);
;; not flattened

(library (host)
  (export embedded-file execute-bytevector bytevector-copy!)
  (import (rename (chezscheme) (bytevector-copy! chez-bytevector-copy!)))


  (define (embedded-file name)
    #f)


  ;; Runs the executable bv from a temporary file, then exits with its status; Chez has no
  ;; execve, so the program's name is the file's, not name
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
        (exit status))))


  ;; The bytes [start, end) of from to to at at, in R7RS's order, not R6RS's
  (define (bytevector-copy! to at from start end)
    (chez-bytevector-copy! from start to at (- end start))))
