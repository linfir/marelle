;; Formatted output, Chez's format, printf and fprintf with the directives ~a, ~s, ~x and ~%

;; Writes the format string fmt with the arguments args to the port
(define (format-to port fmt args)
  (declare unsafe)
  (check-string fmt "`format` expected a string")
  (let ((n (string-length fmt)))
    (let char-next ((i 0)
                    (args args))
      (cond
       ((= i n)
        (unless (null? args)
          (scheme-error "`format` too many arguments for control string" fmt)))
       ((not (eq? (string-ref fmt i) #\~))
        (write-char (string-ref fmt i) port)
        (char-next (+ i 1) args))
       ((= (+ i 1) n) (scheme-error "`format` incomplete format directive" fmt))
       ((eq? (string-ref fmt (+ i 1)) #\%)
        (write-char #\newline port)
        (char-next (+ i 2) args))
       ((not (memv (string-ref fmt (+ i 1)) '(#\a #\s #\x)))
        (scheme-error "`format` unrecognized directive" (string-ref fmt (+ i 1))))
       ((null? args) (scheme-error "`format` too few arguments for control string" fmt))
       (else
        (let ((c (string-ref fmt (+ i 1)))
              (x (car args)))
          (cond
           ((eq? c #\s) (write x port))
           ((and (eq? c #\x) (number? x)) (display (number->string x 16) port))
           (else (display x port))))
        (char-next (+ i 2) (cdr args)))))))

;; A string, or the output to #t (the current output port) or a port, of the format string
(define (format dest . args)
  (declare unsafe public)
  (cond
   ((string? dest)
    (let ((port (open-output-string)))
      (format-to port dest args)
      (get-output-string port)))
   ((and (not dest) (pair? args)) (apply format args))
   ((and (eq? dest #t) (pair? args)) (format-to (current-output-port) (car args) (cdr args)))
   ((and (port? dest) (pair? args)) (format-to dest (car args) (cdr args)))
   (else (scheme-error "`format` expected a string" dest))))

(define (printf fmt . args)
  (declare unsafe public)
  (format-to (current-output-port) fmt args))

(define (fprintf port fmt . args)
  (declare unsafe public)
  (format-to port fmt args))
