;; The defs and data of a compilation, and its state

(library (defs)
  (export make-def def? def-name def-symbol def-params def-result def-inline? def-public? def-native?
          def-procedure? def-unsafe? def-captures def-anonymous? def-extern? def-body def-env def-env-set!
          make-data data? data-name data-symbol data-nwords data-extern?
          def-emitted? def-proc def-code def-params-kinds def-rest? def-required
          procedure-type binary-ops tests low-ops unsafe-bindings
          names symbols current-strings current-entries core-defs inlining unsafe-code?
          fresh-name add-symbol! add-def-symbols! intern-string! add-entries! core-def)
  (import (chezscheme)
          (utils)
          (vars)
          (output))


  ;; Header of procedure objects, procedure-type in lib/objects.ss
  (define procedure-type (* 42 8))


  (define binary-ops
    '((bit-and . and)
      (bit-or . or)
      (raw+ . add)
      (raw- . sub)
      (raw* . mul)
      (raw/ . div)
      (raw% . rem)
      (raw*hu . mulhu)
      (raw<< . sal)
      (raw>> . sar)
      (raw>>u . shr)))


  ;; (test jump-if-true jump-if-false)
  (define tests
    '((raw=  je  jne)
      (raw<  jl  jge)
      (raw<= jle jg)
      (raw>  jg  jle)
      (raw>= jge jl)
      (raw<u  jb  jae)
      (raw<=u jbe ja)
      (raw>u  ja  jbe)
      (raw>=u jae jb)))


  ;; The low operations unsafe defines may call
  (define low-ops
    (append (map car binary-ops)
            (map car tests)
            '(raw as-raw as-val syscall load store load8 store8 copy-words fill-words raw/wu
                  alloc alloc-frame tail-call-frame self address)))


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
  (define-record-type def
    (fields name symbol params result inline? public? native? procedure? unsafe? captures
            anonymous? extern? body (mutable env)))


  (define-record-type data
    (fields name symbol nwords extern?))


  (define (def-emitted? def) (or (def-public? def) (not (def-inline? def))))
  (define (def-proc def) (string-append (def-symbol def) "_proc"))
  (define (def-code def)
    (if (string=? (def-symbol def) "main")
        "scheme_main"
        (string-append (def-symbol def) "_code")))
  (define (def-params-kinds def) (map cdr (def-params def)))
  (define (def-rest? def)
    (and (pair? (def-params def)) (eq? (cdar (last-pair (def-params def))) 'rest)))
  ;; The parameters but the rest one
  (define (def-required def)
    (if (def-rest? def) (- (length (def-params def)) 1) (length (def-params def))))


  ;; Per compilation: the fresh name counter, the symbols (symbol => name), the interned strings
  ;; ((string . name), newest first), the entries of the compiled functions (newest first),
  ;; the library defs the compiler calls (name => def), and the inline defs being expanded
  (define names (make-parameter #f))
  (define symbols (make-parameter #f))
  (define current-strings (make-parameter '()))
  (define current-entries (make-parameter '()))
  (define core-defs (make-parameter '()))
  (define inlining (make-parameter '()))
  ;; True while compiling an unsafe define, which may use strings and low operations
  (define unsafe-code? (make-parameter #f))


  (define (fresh-name x)
    (string->symbol (format "~a.~a" x ((names)))))


  ;; Symbols the IR and the stubs define
  (define reserved-symbols
    '("scheme_main" "scheme_globals" "scheme_nglobals" "_start" "alloc_frame" "alloc_object"))


  ;; Mangling is not injective, so two names giving the same symbol are an error;
  ;; interned strings are strN
  (define (add-symbol! sym name)
    (when (or (hashtable-ref (symbols) sym #f)
              (member sym reserved-symbols)
              (and (> (string-length sym) 3)
                   (string=? (substring sym 0 3) "str")
                   (for-all char-numeric? (string->list (substring sym 3 (string-length sym))))))
      (error 'add-symbol! "symbol clash" name sym))
    (hashtable-set! (symbols) sym name))


  (define (add-def-symbols! def)
    (for-each (lambda (sym) (add-symbol! sym (def-name def)))
              (cond
               ((def-native? def) (list (def-code def)))
               ((string=? (def-symbol def) "main") (list (def-proc def)))
               (else (list (def-code def) (def-proc def))))))


  (define (intern-string! x)
    (cond
     ((assoc x (current-strings)) => cdr)
     (else
      (let ((name (format "str~a" (+ 1 (length (current-strings))))))
        (current-strings (acons x name (current-strings)))
        name))))


  (define (add-entries! entries)
    (current-entries (append (reverse entries) (current-entries))))


  (define (core-def name)
    (cond
     ((assq name (core-defs)) => cdr)
     (else (error 'core-def "missing library def" name)))))
