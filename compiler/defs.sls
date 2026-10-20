;; The defs and data of a compilation, and its state

(library (defs)
  (export make-def def? def-name def-symbol def-params def-result def-inline? def-public? def-noalloc?
          def-procedure? def-unsafe? def-captures def-anonymous? def-extern? def-body def-env def-env-set!
          def-core def-core-set! def-core-names def-core-body
          make-data data? data-name data-symbol data-nwords data-extern?
          def-emitted? def-proc def-code def-params-kinds def-rest? def-required
          procedure-type unsafe-bindings
          names symbols current-strings current-numbers current-literals literals-symbol startup-defs current-entries
          core-defs inlining unsafe-code? general-def
          fresh-name add-symbol! add-def-symbols! intern-string! intern-number! intern-literal! add-entries! core-def)
  (import (chezscheme)
          (utils)
          (vars)
          (ops)
          (output))


  ;; Header of procedure objects, procedure-type in lib/objects.ss
  (define procedure-type (* 42 8))


  ;; bignum-type and ratnum-type in lib/objects.ss
  (define bignum-type (* 46 8))
  (define ratnum-type (* 55 8))


  ;; What only unsafe defines see, after their other names
  (define unsafe-bindings
    (append (map (lambda (op) (cons op (make-low-op-binding op))) low-ops)
            (list (cons 'undef (make-constant-binding (immediate 'undef))))))


  ;; name       the Scheme name
  ;; symbol     the mangled name, prefix of the def's symbols
  ;; params     (name . kind) pairs
  ;; result     kind
  ;; procedure? lifted like a lambda: Scheme semantics for its parameters (boxes)
  ;; captures   number of captures, 0 for a static procedure
  ;; anonymous? the procedure object has no name
  ;; extern?    emitted in another object (the library, for a program)
  ;; env        the env of the body, set once every name is bound
  ;; core       (names . body): the parameters' names and the body after the front pass
  (define-record-type def
    (fields name symbol params result inline? public? noalloc? procedure? unsafe? captures
            anonymous? extern? body (mutable env) (mutable core)))


  (define-record-type data
    (fields name symbol nwords extern?))



  (define (def-emitted? def) (or (def-public? def) (not (def-inline? def))))
  (define (def-proc def) (string-append (def-symbol def) "_proc"))
  (define (def-code def)
    (if (string=? (def-symbol def) "main")
        "scheme_main"
        (string-append (def-symbol def) "_code")))
  (define (def-params-kinds def) (map cdr (def-params def)))
  (define (def-core-names def) (car (def-core def)))
  (define (def-core-body def) (cdr (def-core def)))
  (define (def-rest? def)
    (and (pair? (def-params def)) (eq? (cdar (last-pair (def-params def))) 'rest)))
  ;; The parameters but the rest one
  (define (def-required def)
    (if (def-rest? def) (- (length (def-params def)) 1) (length (def-params def))))


  ;; Per compilation: the fresh name counter, the symbols (symbol => name), the interned strings
  ;; ((string . name), newest first), the static numbers ((n name . entry), newest first), the
  ;; string and symbol literals ((datum . cell index), newest first)
  ;; and the data of their cells, the defs main calls first, the entries of the compiled functions
  ;; (newest first), the library defs the compiler calls (name => def), and the inline defs being
  ;; expanded
  (define names (make-parameter #f))
  (define symbols (make-parameter #f))
  (define current-strings (make-parameter '()))
  (define current-numbers (make-parameter '()))
  (define current-literals (make-parameter '()))
  (define literals-symbol (make-parameter #f))
  (define startup-defs (make-parameter '()))
  (define current-entries (make-parameter '()))
  (define core-defs (make-parameter '()))
  (define inlining (make-parameter '()))
  ;; True while compiling an unsafe define, which may use C strings and low operations
  (define unsafe-code? (make-parameter #f))


  (define (fresh-name x)
    (string->symbol (format "~a.~a" x ((names)))))


  ;; Symbols the IR and the stubs define
  (define reserved-symbols
    '("scheme_main" "scheme_globals" "scheme_nglobals" "scheme_literals" "scheme_nliterals"
      "scheme_library_literals" "scheme_library_nliterals" "_start" "alloc_frame" "alloc_object"))


  ;; Mangling is not injective, so two names giving the same symbol are an error;
  ;; interned strings are strN, static numbers numN
  (define (add-symbol! sym name)
    (when (or (hashtable-ref (symbols) sym #f)
              (member sym reserved-symbols)
              (and (> (string-length sym) 3)
                   (member (substring sym 0 3) '("str" "num"))
                   (for-all char-numeric? (string->list (substring sym 3 (string-length sym))))))
      (compile-error 'add-symbol! "symbol clash" name sym))
    (hashtable-set! (symbols) sym name))


  (define (add-def-symbols! def)
    (for-each (lambda (sym) (add-symbol! sym (def-name def)))
              (cond
               ((def-noalloc? def) (list (def-code def)))
               ((string=? (def-symbol def) "main") (list (def-proc def)))
               (else (list (def-code def) (def-proc def))))))


  (define (intern-string! x)
    (cond
     ((assoc x (current-strings)) => cdr)
     (else
      (let ((name (format "str~a" (+ 1 (length (current-strings))))))
        (current-strings (acons x name (current-strings)))
        name))))


  ;; The static bignum or ratnum n in read-only data: [bignum-type, nlimbs, sign, limb ...] or
  ;; [ratnum-type, numerator, denominator], each a fixnum or a static bignum
  (define (intern-number! n)
    (define (word x)
      (if (<= (- (expt 2 60)) x (- (expt 2 60) 1))
          (+ (* x 8) 1)
          (intern-number! x)))
    (cond
     ((assv n (current-numbers)) => cadr)
     (else
      (let* ((entry (if (integer? n)
                        (let ((limbs (let loop ((m (abs n)))
                                       (if (= m 0)
                                           '()
                                           (cons (bitwise-and m (- (expt 2 64) 1))
                                                 (loop (bitwise-arithmetic-shift-right m 64)))))))
                          `((quad ,bignum-type) (quad ,(length limbs)) (quad ,(if (< n 0) 1 0))
                            ,@(map (lambda (l) `(quad ,l)) limbs)))
                        `((quad ,ratnum-type) (quad ,(word (numerator n)))
                          (quad ,(word (denominator n))))))
             (name (format "num~a" (+ 1 (length (current-numbers))))))
        (current-numbers (cons (cons* n name `(rodata ,name ,@entry)) (current-numbers)))
        name))))


  ;; The index of the literal cell of the Scheme string, symbol, pair, vector or bytevector x; the strings
  ;; and symbols it contains, a symbol its name, get their cells first, so they are filled first
  (define (intern-literal! x)
    (cond
     ((assoc x (current-literals)) => cdr)
     (else
      (let walk ((y (if (symbol? x) (symbol->string x) x)))
        (cond
         ((pair? y) (walk (car y)) (walk (cdr y)))
         ((vector? y) (vector-for-each walk y))
         ((and (or (string? y) (symbol? y)) (not (eq? y x))) (intern-literal! y))))
      (let ((i (length (current-literals))))
        (current-literals (acons x i (current-literals)))
        i))))


  (define (add-entries! entries)
    (current-entries (append (reverse entries) (current-entries))))


  (define (core-def name)
    (cond
     ((assq name (core-defs)) => cdr)
     (else (compile-error 'core-def "missing library def" name))))


  ;; The general def variadic-D of the library def D, a procedure taking any number of
  ;; arguments, of which D is a fixed-arity specialization
  (define (general-def def)
    (let ((name (def-name def)))
      (and (eq? (cond ((assq name (core-defs)) => cdr) (else #f)) def)
           (cond
            ((assq (string->symbol (format "variadic-~a" name)) (core-defs)) => cdr)
            (else #f)))))


  ;; Printed by name, in dumps
  (add-printer! def? def-name)
  (add-printer! data? data-name))
