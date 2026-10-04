(define (proc-name n)
  (string-append "proc" (number->string n)))


(define (fn-name n)
  (string-append "fn" (number->string n)))


(define (prelude-proc-name n)
  (string-append "prelude_proc" (number->string n)))


(define (prelude-fn-name n)
  (string-append "prelude_fn" (number->string n)))


(define (prim-proc-name name)
  (string-append name "_proc"))


(define (prim-fn-name name)
  (string-append name "_code"))


;; procedure object: [type, arity, code, ncaptures, captures...]
(define (proc-data proc arity code)
  (list 'data proc
        (list 'quad "procedure_type")
        (list 'quad arity)
        (list 'quad code)
        (list 'quad 0)))


(define int-bits 61)
(define int-min (- (expt 2 (- int-bits 1))))
(define int-max (- (expt 2 (- int-bits 1)) 1))


(define undef (void))


(define (immediate? x)
  (or (and (integer? x)
           (exact? x)
           (<= int-min x int-max))
      (boolean? x)
      (equal? x ''())
      (eq? x undef)))


(define (encode-immediate x)
  (define (tag t x)
    (+ t (* x 8)))
  (cond
   ((and (integer? x) (exact? x))
    (tag 1 x))
   ((boolean? x)
    (tag 2 (if x 1 0)))
   ((equal? x ''())
    (tag 2 2))
   ((eq? x undef)
    (tag 2 3))
   (else (error 'encode-immediate "non immediate value" x))))


(define (print-instr instr)
  (if (eq? (car instr) 'def)
      (printf "  ~s\n" instr)
      (printf "    ~s\n" instr)))


(define (print-top x)
  (case (car x)
    ((data cells)
     (printf "~s\n\n" x))
    ((function)
     (printf "(function ~s ~s\n" (cadr x) (caddr x))
     (for-each print-instr (cdddr x))
     (display ")\n\n"))))


(define (print-ir tops)
  (for-each print-top tops))
