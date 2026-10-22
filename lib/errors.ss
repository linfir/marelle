;; Chez's error and errorf: print the message to stderr in Chez's wording, then exit 1

;; Checks who, a symbol, a string or #f, and the message, a string
(define (check-error-args who msg irritants who-name)
  (declare inline (result void))
  (unless (or (not who) (symbol? who) (string? who))
    (errorf who-name "invalid who argument ~s (message = ~s, irritants = ~s)" who msg irritants))
  (unless (string? msg)
    (errorf who-name "invalid message argument ~s (who = ~s, irritants = ~s)" msg who irritants)))

;; Writes "Exception: " or "Exception in who: " to stderr, a procedure by its name
(define (write-error-who who)
  (declare unsafe inline (result void))
  (put-string stderr "Exception")
  (when who
    (put-string stderr " in "))
  (when (procedure? who)
    (put-string stderr (load (as-raw who) (raw 3))))
  (when (and who (not (procedure? who)))
    (display who (as-val stderr)))
  (put-string stderr ": "))

;; "Exception in who: msg", then " with irritant x" or " with irritants (x ...)"
(define (error who msg . irritants)
  (declare unsafe public)
  (check-error-args who msg irritants 'error)
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
  (check-error-args who fmt args 'errorf)
  (write-error-who who)
  (format-to (as-val stderr) fmt args)
  (newline (as-val stderr))
  (exit-process (raw 1)))

;; errorf for the library, args a list, whose who may also be a procedure of the library, named: a static
;; procedure object needs no literal cell, so the error paths add nothing to the startup
(define (library-errorf who fmt args)
  (declare unsafe (result never))
  (write-error-who who)
  (format-to (as-val stderr) fmt args)
  (newline (as-val stderr))
  (exit-process (raw 1)))

;; The library's errors, in Chez's wording: the argument x of who, of the kind of error
(define (argument-error who kind x)
  (declare (result never))
  (library-errorf who
                  (case kind
                    ((pair) "~s is not a pair")
                    ((mutable-pair) "~s is not a mutable pair")
                    ((list) "~s is not a proper list")
                    ((vector) "~s is not a vector")
                    ((mutable-vector) "~s is not a mutable vector")
                    ((vector-length) "~s is not a valid vector length")
                    ((string) "~s is not a string")
                    ((char) "~s is not a character")
                    ((mutable-string) "~s is not a mutable string")
                    ((string-length) "~s is not a valid string length")
                    ((scalar-value) "~s is not a valid unicode scalar value")
                    ((symbol) "~s is not a symbol")
                    ((procedure) "~s is not a procedure")
                    ((number) "~s is not a number")
                    ((real) "~s is not a real number")
                    ((rational) "~s is not a rational number")
                    ((integer) "~s is not an integer")
                    ((exact-integer) "~s is not an exact integer")
                    ((nonnegative-exact-integer) "~s is not a nonnegative exact integer")
                    ((fixnum) "~s is not a fixnum")
                    ((nonnegative-fixnum) "~s is not a nonnegative fixnum")
                    ((radix) "~s is not a valid radix")
                    ((undefined) "undefined for ~s")
                    ((size) "invalid size ~s")
                    ((index) "invalid index ~s")
                    ((size-argument) "invalid size argument ~s")
                    ((bit-index) "invalid bit index ~s")
                    ((bytevector) "~s is not a bytevector")
                    ((mutable-bytevector) "~s is not a mutable bytevector")
                    ((bytevector-length) "~s is not a valid bytevector length")
                    ((fill-value) "~s is not a valid fill value")
                    ((value) "invalid value ~s")
                    ((hashtable) "~s is not a hashtable")
                    ((flexvector) "~s is not a flexvector")
                    ((nonempty-flexvector) "~s is not a nonempty flexvector")
                    ((port) "~s is not a port")
                    ((input-port) "~s is not an input port")
                    ((output-port) "~s is not an output port")
                    ((textual-input-port) "~s is not a textual input port")
                    ((textual-output-port) "~s is not a textual output port")
                    ((binary-input-port) "~s is not a binary input port")
                    ((binary-output-port) "~s is not a binary output port")
                    ((octet) "~s is not an octet")
                    ((string-output-port) "~s is not a string output port")
                    ((closed-port) "not permitted on closed port ~s"))
                  (list x)))

;; Native, since make-call-frame is inlined by the frames pass, which makes no more calls
(define (non-procedure-error x)
  (declare unsafe noalloc (result never))
  (put-string stderr "Exception: attempt to apply non-procedure ")
  (put-datum stderr (raw 1) x)
  (put-string stderr "\n")
  (exit-process (raw 1)))

;; i out of the range of x, a vector, a string, a bytevector or a flexvector
(define (index-error who i x)
  (declare (result never))
  (library-errorf who "~s is not a valid index for ~s" (list i x)))

;; start and end out of the range of x, a string or a bytevector
(define (range-error who start end x)
  (declare (result never))
  (library-errorf who "~s and ~s are not valid start/end indices for ~s" (list start end x)))

;; The procedure who, or an unnamed one for #f, called with n arguments
(define (argument-count-error who n)
  (declare (result never))
  (if who
      (library-errorf #f "incorrect number of arguments ~s to ~s" (list n who))
      (library-errorf #f "incorrect number of arguments ~s to #<procedure>" (list n))))

;; Not who, which the hashtable operations do not pass down
(define (hash-function-error hash h k)
  (declare (result never))
  (library-errorf #f "invalid hash-function ~s return value ~s for ~s" (list hash h k)))
