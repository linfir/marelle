;; Chez's error and errorf: print the message to stderr in Chez's wording, then exit 1

;; Checks who, a symbol, a string or #f, and the message, a string
(define (check-error-args who msg (raw who-msg) (raw msg-msg))
  (declare unsafe inline (result void))
  (unless (or (not who) (symbol? who) (string? who))
    (scheme-error who-msg who))
  (check-string msg msg-msg))

;; Writes "Exception: " or "Exception in who: " to stderr
(define (write-error-who who)
  (declare unsafe inline (result void))
  (put-string stderr "Exception")
  (when who
    (put-string stderr " in ")
    (display who (as-val stderr)))
  (put-string stderr ": "))

;; "Exception in who: msg", then " with irritant x" or " with irritants (x ...)"
(define (error who msg . irritants)
  (declare unsafe public)
  (check-error-args who msg "`error` invalid who argument" "`error` invalid message argument")
  (write-error-who who)
  (display msg (as-val stderr))
  (cond
   ((null? irritants))
   ((null? (cdr irritants))
    (put-string stderr " with irritant ")
    (write (car irritants) (as-val stderr)))
   (else
    (put-string stderr " with irritants ")
    (write irritants (as-val stderr))))
  (newline (as-val stderr))
  (exit-process (raw 1)))

;; "Exception in who: " and the format string fmt with the arguments args
(define (errorf who fmt . args)
  (declare unsafe public)
  (check-error-args who fmt "`errorf` invalid who argument" "`errorf` invalid message argument")
  (write-error-who who)
  (format-to (as-val stderr) fmt args)
  (newline (as-val stderr))
  (exit-process (raw 1)))
