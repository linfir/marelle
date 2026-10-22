;; Compiling the library and programs, and the interface between them

(library (library)
  (export compile-scheme-library compile-scheme-program)
  (import (chezscheme)
          (utils)
          (parse)
          (front)
          (letrec)
          (vars)
          (closure)
          (resolve)
          (assign)
          (core-check)
          (defs)
          (convert)
          (inline)
          (output))


  (define (item-name x)
    (cond
     ((def? x) (def-name x))
     ((data? x) (data-name x))
     (else (library-constant-name x))))


  (define (item-binding x)
    (cond
     ((def? x) (make-def-binding x))
     ((data? x) (make-data-binding x))
     ((eq? (library-constant-kind x) 'raw)
      (make-constant-binding `(quad ,(library-constant-value x))))
     (else (make-constant-binding (immediate (library-constant-value x))))))


  ;; Data, constants, and defs with raw parameters or result, only unsafe defines may use
  (define (unsafe-item? x)
    (or (data? x)
        (library-constant? x)
        (eq? (def-result x) 'raw)
        (memq 'raw (def-params-kinds x))
        #f))


  ;; The bindings of the safe items, then those of the unsafe ones
  (define (item-bindings items)
    (values (map (lambda (x) (cons (item-name x) (item-binding x)))
                 (filter (lambda (x) (not (unsafe-item? x))) items))
            (map (lambda (x) (cons (item-name x) (item-binding x)))
                 (filter unsafe-item? items))))


  ;; The def, data or constant of a library define, define-data or define-constant; a procedure
  ;; gets a fresh symbol, since programs may have their own of the same name
  (define (scanned-item x)
    (cond
     ((library-constant? x) x)
     ((library-data? x)
      (let ((d (make-data (library-data-name x) (mangle (library-data-name x))
                          (library-data-nwords x) #f)))
        (add-symbol! (data-symbol d) (data-name d))
        d))
     (else
      (let* ((name (library-def-name x))
             (attrs (library-def-attrs x))
             (def (make-def name
                            (mangle (if (library-def-procedure? x) (fresh-name name) name))
                            (map cons (library-def-args x) (library-def-kinds x))
                            (library-def-result x)
                            (and (memq ':inline attrs) #t) (and (memq ':public attrs) #t)
                            (and (memq ':noalloc attrs) #t) (and (memq ':pure attrs) #t)
                            (library-def-procedure? x)
                            (library-def-unsafe? x) 0 #f #f (library-def-body x) #f #f)))
        (when (and (def-noalloc? def) (or (def-inline? def) (def-public? def)))
          (compile-error 'scanned-item ":noalloc with another attribute" name))
        (when (and (def-pure? def) (not (def-noalloc? def)))
          (compile-error 'scanned-item ":pure without :noalloc" name))
        ;; An emitted def is called with the Scheme convention
        (unless (or (not (def-emitted? def))
                    (def-noalloc? def)
                    (and (for-all (lambda (k) (memq k '(val rest))) (def-params-kinds def))
                         (memq (def-result def) '(val never))))
          (compile-error 'scanned-item "raw parameter or result in a non-inline def" name))
        (add-def-symbols! def)
        def))))


  ;; The library defs a letrec init may call and stay simple
  (define pure-names
    '(car cdr cons + - * = < > <= >= eq? eqv? not null? pair? boolean? integer? procedure?
          char? string? symbol? vector? zero? make-record-type))


  ;; Per compilation: the library's defs of pure-names
  (define pure-defs (make-parameter '()))


  (define (set-pure-defs! library)
    (pure-defs (filter (lambda (x) (and (def? x) (memq (def-name x) pure-names))) library)))


  ;; x a name, or a (core name) head, always the library's def
  (define (pure-def-name? env x)
    (if (pair? x)
        (and (memq (cadr x) pure-names) #t)
        (let ((entry (assq x env)))
          (and entry
               (def-binding? (cdr entry))
               (memq (def-binding-def (cdr entry)) (pure-defs))
               #t))))


  ;; The core of a def, its letrec* forms fixed, analyzed, its closures converted, its names
  ;; resolved, its assignments converted, checked
  (define (make-core name env names body unsafe?)
    (let* ((core (dumped 'front (list name) (front-def env names body unsafe?)))
           (core (dumped 'letrec (list name)
                         (cons (car core)
                               (fix-letrec (cdr core) (lambda (x) (pure-def-name? env x)))))))
      (analyze-def! (car core) (cdr core))
      (let ((core (dumped 'closure (list name) (cons (car core) (convert-closures (cdr core))))))
        (checked name
                 (dumped 'assign (list name)
                         (convert-assignments
                          (dumped 'resolve (list name)
                                  (cons (car core) (resolve-body env (cdr core))))))))))


  (define (checked name core)
    (check-core name core)
    core)


  ;; The core of main, whose body is a letrec*: its variables not bound by a fix get global
  ;; cells
  (define (make-main-core env body)
    (let* ((core (dumped 'front '(main) (front-def env '() body #f)))
           (vars (map car (cadr (cadr core))))
           (core (dumped 'letrec '(main)
                         (cons '() (fix-letrec (cdr core) (lambda (x) (pure-def-name? env x))))))
           (fixed (fix-names core)))
      (for-each (lambda (x)
                  (unless (memq x fixed)
                    (hashtable-set! (top-globals) x (hashtable-size (top-globals)))))
                vars)
      (analyze-def! '() (cdr core))
      (let ((core (dumped 'closure '(main) (cons '() (convert-closures (cdr core))))))
        (checked 'main
                 (dumped 'assign '(main)
                         (convert-assignments
                          (dumped 'resolve '(main) (cons '() (resolve-body env (cdr core))))))))))


  (define (fix-names x)
    (cond
     ((not (pair? x)) '())
     ((eq? (special-keyword x) 'fix) (append (map car (cadr x)) (fix-names (cdr x))))
     (else (append (fix-names (car x)) (fix-names (cdr x))))))


  ;; Each def's env: env, or unsafe-env for an unsafe def, then its core when it has a body
  (define (set-envs! items env unsafe-env)
    (for-each (lambda (x)
                (when (def? x)
                  (def-env-set! x (if (def-unsafe? x) unsafe-env env))
                  (unless (null? (def-body x))
                    (def-core-set! x (make-core (def-name x) (def-env x) (map car (def-params x))
                                                (def-body x) (def-unsafe? x))))))
              items))


  ;; The emitted defs that are not extern, compiled in their env
  (define (compile-defs! items)
    (for-each (lambda (x)
                (when (and (def? x) (def-emitted? x) (not (def-extern? x)))
                  (if (def-procedure? x)
                      (compile-procedure x '())
                      (compile-low-def x))))
              items))


  ;; The symbols an entry defines
  (define (entry-symbols entry)
    (if (memq (car entry) '(rodata function noalloc))
        (list (cadr entry))
        '()))


  (define (constant-entries)
    (append (map (lambda (s) `(string ,(cdr s) ,(car s)))
                 (reverse (current-strings)))
            (map cddr (reverse (current-numbers)))))


  ;; The cells of the literals, 0 until the init function fills them, and their count when
  ;; count-symbol is not #f
  (define (literal-entries symbol count-symbol)
    (let ((n (length (current-literals))))
      (cons `(data ,symbol ,n 0)
            (if count-symbol (list `(rodata ,count-symbol (quad ,n))) '()))))


  ;; The def of the function filling a unit's literal cells, called by main
  (define (literal-init-def symbol extern?)
    (make-def 'literal-init symbol '() 'val #f #f #f #f #f #t 0 #t extern? '() #f #f))


  (define (data-entries items)
    (map (lambda (d) `(data ,(data-symbol d) ,(data-nwords d) 0))
         (filter (lambda (x) (and (data? x) (not (data-extern? x)))) items)))


  (define (new-compilation first-name literals thunk)
    (parameterize ((names (make-counter first-name))
                   (symbols (make-hashtable string-hash string=?))
                   (current-strings '())
                   (current-numbers '())
                   (current-literals '())
                   (literals-symbol literals)
                   (startup-defs '())
                   (current-entries '())
                   (inlining '())
                   (core-defs '())
                   (var-facts (make-eq-hashtable))
                   (lambda-frees (make-eq-hashtable))
                   (top-globals (make-eq-hashtable))
                   (pure-defs '()))
      (thunk)))


  ;; The IR entries of the library, its forms (the files of lib/), and the interface a program
  ;; compiles against: (names n), where a program's fresh names start, then each def, inline
  ;; ones with their body, and each data
  (define (compile-scheme-library forms)
    (new-compilation
     0 "scheme_library_literals"
     (lambda ()
       (let ((items (map-in-order scanned-item (scan-library forms)))
             (init (literal-init-def "scheme_library_init" #f)))
         (add-def-symbols! init)
         (set-pure-defs! items)
         (core-defs (map (lambda (x) (cons (item-name x) x)) (filter def? items)))
         (let-values (((safe unsafe) (item-bindings items)))
           (set-envs! items safe (append safe unsafe unsafe-bindings)))
         (let ((funs (compile-unit (lambda () (compile-defs! items)))))
           (compile-literal-init init)
           (values
            (let ((entries (reverse (current-entries))))
              `((export ,@(apply append (map entry-symbols entries))
                        ,@(map data-symbol (filter data? items)))
                ,@(data-entries items)
                ,@(literal-entries "scheme_library_literals" "scheme_library_nliterals")
                ,@(constant-entries)
                ,@entries
                (stubs)))
            `((names ,((names)))
              ,@(map (lambda (x)
                       (cond
                        ((library-constant? x)
                         `(constant ,(library-constant-name x) ,(library-constant-kind x)
                                    ,(library-constant-value x)))
                        ((data? x)
                         `(data ,(data-name x) ,(data-symbol x) ,(data-nwords x)))
                        (else
                         `(def ,(def-name x) ,(def-symbol x) ,(def-params x) ,(def-result x)
                               (,@(if (def-inline? x) '(inline) '())
                                ,@(if (def-public? x) '(public) '())
                                ,@(if (def-noalloc? x) '(noalloc) '())
                                ,@(if (def-pure? x) '(pure) '()))
                               ,(def-unsafe? x) ,(def-procedure? x)
                               ,@(if (def-inline? x) (def-body x) '())))))
                     items)
              ,@(cps-entries funs items))))))))


  ;; (cps name fun) for the funs a program may inline, those of items naming only items
  (define (cps-entries funs items)
    (let ((item? (let ((t (make-eq-hashtable)))
                   (for-each (lambda (x) (when (def? x) (hashtable-set! t x #t))) items)
                   (lambda (def) (hashtable-ref t def #f)))))
      (filter (lambda (x) x)
              (map (lambda (f)
                     (let ((fun (and (item? (car f)) (export-fun (car f) (cdr f) item?))))
                       (and fun `(cps ,(def-name (car f)) ,fun))))
                   funs))))


  ;; The funs of the interface's cps entries, an eq hashtable from the library's defs; those
  ;; naming a def or a symbol the library does not export are left out
  (define (interface-funs interface library)
    (let ((defs (make-eq-hashtable))
          (symbols (make-hashtable string-hash string=?))
          (funs (make-eq-hashtable)))
      (for-each (lambda (x) (when (def? x) (hashtable-set! defs (def-name x) x))) library)
      (for-each (lambda (x)
                  (case (car x)
                    ((symbol) (hashtable-set! symbols (cadr x) #t))
                    ((export) (for-each (lambda (s) (hashtable-set! symbols s #t)) (cdr x)))))
                interface)
      (for-each (lambda (x)
                  (when (eq? (car x) 'cps)
                    (let ((def (hashtable-ref defs (cadr x) #f))
                          (fun (import-fun (caddr x)
                                           (lambda (name) (hashtable-ref defs name #f))
                                           (lambda (s) (hashtable-ref symbols s #f)))))
                      (when (and def fun)
                        (hashtable-set! funs def fun)))))
                interface)
      funs))


  ;; The def, data or constant of an interface entry, extern
  (define (interface-item x)
    (case (car x)
      ((constant) (apply make-library-constant (cdr x)))
      ((data) (apply (lambda (name symbol nwords) (make-data name symbol nwords #t)) (cdr x)))
      (else
       (apply (lambda (name symbol params result attrs unsafe? procedure? . body)
                (make-def name symbol params result (and (memq 'inline attrs) #t)
                          (and (memq 'public attrs) #t) (and (memq 'noalloc attrs) #t)
                          (and (memq 'pure attrs) #t) procedure? unsafe? 0 #f #t body #f #f))
              (cdr x)))))


  (define (define-data-form? form)
    (and (non-empty-list? form) (eq? (car form) 'define-data)))


  (define (static-form? form)
    (or (declared-define? form) (define-data-form? form) (define-constant-form? form)))


  ;; Also expands define-values and define-record-type
  (define (splice-begins forms)
    (apply append (map (lambda (x)
                         (cond
                          ((and (non-empty-list? x) (eq? (car x) 'begin))
                           (splice-begins (cdr x)))
                          ((define-values-form? x)
                           (expand-top-define-values x))
                          ((define-record-type-form? x)
                           (expand-top-define-record-type x))
                          (else (list x))))
                       forms)))


  ;; The body of main: the forms as a letrec*, each expression bound to an unused variable,
  ;; as in R6RS, but the last form when result?, which gives the result, else undef
  (define (main-body forms result?)
    (let loop ((forms forms)
               (bindings '()))
      (cond
       ((null? forms)
        `((,(marker 'letrec*) ,(reverse bindings) ,undef)))
       ((and result? (null? (cdr forms)))
        `((,(marker 'letrec*) ,(reverse bindings) ,(car forms))))
       ((define-form? (car forms))
        (let-values (((name args body) (parse-define (car forms))))
          (when (assq name bindings)
            (compile-error 'compile-scheme-program "redefinition not allowed" name))
          (loop (cdr forms)
                (cons (list name (if args `(,(marker 'lambda) ,args ,@body) (car body)))
                      bindings))))
       (else
        (loop (cdr forms) (cons (list (gensym "expression") (car forms)) bindings))))))


  ;; The table of the embedded files, ((name path bytes) ...):
  ;; their count, then per file its name (a C string) and its static bytevector; then the bytevectors
  (define (embedded-entries embedded)
    (let loop ((l embedded) (i 1) (table '()) (files '()))
      (if (null? l)
          (cons `(rodata "scheme_embedded" (quad ,(length embedded)) ,@(reverse table))
                (reverse files))
          (let ((symbol (format "scheme_embedded_~a" i))
                (name (intern-string! (caar l))))
            (add-symbol! symbol symbol)
            (loop (cdr l) (+ i 1)
                  (append (reverse `((quad ,name) (quad ,symbol))) table)
                  (cons `(bytes ,symbol (,immutable-bytevector-type ,(bytevector-length (caddr (car l))))
                                ,(cadr (car l)) ,(caddr (car l)))
                        files))))))


  ;; The program's embedded datums, ((name . forms) ...), as static immutable objects in one
  ;; blob, scheme_static_data, each referenced as the blob's address plus its offset, plus 7 for an
  ;; immutable pair; strings and symbols shared. => the entries of the blob, of the table of the datums
  ;; (scheme_program's word 8: a count, then each name and datum), of the table of the symbols
  ;; (word 7: a count, then each symbol), which intern-static-symbols! enters at startup; and
  ;; whether there are symbols
  (define (static-entries datums)
    (let ((words (make-eqv-hashtable))
          (size 0)
          (strings (make-hashtable string-hash string=?))
          (symbols (make-eq-hashtable))
          (symbol-refs '()))
      (define (reserve! n)
        (let ((i size))
          (set! size (+ size n))
          i))

      (define (set-word! i w)
        (hashtable-set! words i w))

      ;; The UTF-8 strings, in a writable blob of their own, since indexing writes their cache
      (define utf8-words '())
      (define utf8-size 0)

      (define (ref i tag)
        `(+ "scheme_static_data" ,(+ (* 8 i) tag)))

      (define (place x)
        (cond
         ((pair? x) (place-list x))
         ((null? x) (immediate-value ''()))
         ((string? x) (place-string x))
         ((symbol? x) (place-symbol x))
         ((vector? x) (place-vector x))
         ((and (exact-rational? x) (not (fixnum-value? x))) (intern-number! x))
         ((or (fixnum-value? x) (boolean? x) (char? x)) (immediate-value x))
         (else (compile-error 'static-entries "unsupported datum to embed" x))))

      (define (immediate-value x)
        (cadadr (immediate x)))

      ;; Along the spine, so a long list does not nest
      (define (place-list x)
        (let ((first (reserve! 2)))
          (let loop ((i first) (x x))
            (set-word! i (place (car x)))
            (if (pair? (cdr x))
                (let ((j (reserve! 2)))
                  (set-word! (+ i 1) (ref j 7))
                  (loop j (cdr x)))
                (set-word! (+ i 1) (place (cdr x)))))
          (ref first 7)))

      ;; ASCII, [header, nchars, bytes...], else UTF-8, [header, nchars, nbytes, cache-index,
      ;; cache-offset, bytes...] (lib/strings.ss)
      (define (place-string s)
        (or (hashtable-ref strings s #f)
            (let* ((bytes (bytevector->u8-list (string->utf8 s)))
                   (ascii? (= (length bytes) (string-length s)))
                   (units (let loop ((bytes bytes))
                            (if (null? bytes)
                                '()
                                (let ((n (min 8 (length bytes))))
                                  (cons (fold-right (lambda (b w) (+ b (bitwise-arithmetic-shift-left w 8)))
                                                    0 (list-head bytes n))
                                        (loop (list-tail bytes n)))))))
                   (r (if ascii?
                          (let ((i (reserve! (+ 2 (length units)))))
                            (set-word! i immutable-ascii-string-type)
                            (set-word! (+ i 1) (string-length s))
                            (for-each (lambda (w k) (set-word! (+ i 2 k) w)) units (iota (length units)))
                            (ref i 0))
                          (let ((i utf8-size))
                            (set! utf8-words
                                  (append (reverse units)
                                          (list 0 0 (length bytes) (string-length s) immutable-utf8-string-type)
                                          utf8-words))
                            (set! utf8-size (+ utf8-size 5 (length units)))
                            `(+ "scheme_static_utf8" ,(* 8 i))))))
              (hashtable-set! strings s r)
              r)))

      (define (place-symbol x)
        (or (hashtable-ref symbols x #f)
            (let* ((name (symbol->string x))
                   (i (reserve! 3))
                   (r (ref i 0)))
              (hashtable-set! symbols x r)
              (set! symbol-refs (cons r symbol-refs))
              (set-word! i symbol-type)
              (set-word! (+ i 1) (static-symbol-hash name))
              (set-word! (+ i 2) (place-string name))
              r)))

      (define (place-vector v)
        (let ((i (reserve! (+ 2 (vector-length v)))))
          (set-word! i immutable-vector-type)
          (set-word! (+ i 1) (vector-length v))
          (for-each (lambda (k) (set-word! (+ i 2 k) (place (vector-ref v k))))
                    (iota (vector-length v)))
          (ref i 0)))

      (let* ((table (map-in-order (lambda (d) (list (place-string (car d)) (place (cdr d)))) datums))
             (symbol-refs (reverse symbol-refs)))
        (values
         `(,@(if (zero? size)
                 '()
                 `((rodata "scheme_static_data"
                           ,@(map (lambda (i) `(quad ,(hashtable-ref words i #f))) (iota size)))))
           ,@(if (zero? utf8-size)
                 '()
                 `((words "scheme_static_utf8" ,@(reverse utf8-words))))
           (rodata "scheme_embedded_data" (quad ,(length datums))
                   ,@(map (lambda (w) `(quad ,w)) (apply append table)))
           (rodata "scheme_static_symbols" (quad ,(length symbol-refs))
                   ,@(map (lambda (r) `(quad ,r)) symbol-refs)))
         (pair? symbol-refs)))))


  (define (fixnum-value? x)
    (and (integer? x) (exact? x) (<= (- (expt 2 60)) x (- (expt 2 60) 1))))


  ;; The hash of the string s, as string-hash-word (lib/symbols.ss) computes it at run time
  (define (static-symbol-hash s)
    (fold-left (lambda (h c) (modulo (* (+ h (char->integer c) 1) #x9E3779B97F4A7C15) (expt 2 64)))
               0 (string->list s)))


  ;; The IR entries of the program, its top-level forms with begins spliced: a define with a declare
  ;; define-data and define-constant are static, like the library's; the others are the letrec* body of main
  ;; (main-body), whose variables, but the procedures, are global cells; the last form gives the
  ;; result, which the executable prints when display?; embedded, ((name path bytes) ...), the files
  ;; embedded-file returns; datums, ((name . forms) ...), those embedded-datum returns
  (define (compile-scheme-program interface program embedded datums display?)
    (when (null? (splice-begins program))
      (compile-error 'compile-scheme-program "empty program"))
    (new-compilation
     (cadr (assq 'names interface)) "scheme_literals"
     (lambda ()
       (let ((library (map interface-item
                           (filter (lambda (x) (memq (car x) '(def data constant))) interface)))
             (library-init (literal-init-def "scheme_library_init" #t))
             (init (literal-init-def "scheme_init" #f)))
         (add-symbol! "main_proc" 'main)
         (add-def-symbols! library-init)
         (add-def-symbols! init)
         (set-pure-defs! library)
         (core-defs (map (lambda (x) (cons (def-name x) x)) (filter def? library)))
         (for-each (lambda (x)
                     (cond
                      ((def? x) (add-def-symbols! x))
                      ((data? x) (add-symbol! (data-symbol x) (data-name x)))))
                   library)
         (let-values (((safe unsafe) (item-bindings library)))
           (parameterize ((dump-pass #f))
             (set-envs! library safe (append safe unsafe unsafe-bindings))))
         (let*-values (((all) (splice-begins program))
                       ((static) (filter static-form? all))
                       ((forms) (filter (lambda (x) (not (memq x static))) all))
                       ((items) (map-in-order scanned-item (scan-library static)))
                       ((safe unsafe) (item-bindings items)))
           (for-each (lambda (x)
                       (let-values (((name args body) (parse-define x)))
                         (when (find (lambda (x) (eq? (item-name x) name)) items)
                           (compile-error 'compile-scheme-program "redefinition not allowed" name))))
                     (filter define-form? forms))
           (let* ((env (append safe
                               (map (lambda (x) (cons (def-name x) (make-def-binding x)))
                                    (filter (lambda (x) (and (def? x) (def-public? x))) library))))
                  (unsafe-env (append env unsafe
                                      (map (lambda (x) (cons (item-name x) (item-binding x)))
                                           library)
                                      unsafe-bindings)))
             (set-envs! items env unsafe-env)
             (let ((statics #f))
               (parameterize ((extern-funs (interface-funs interface library))
                              (extern-defs (filter def? library)))
                 (compile-unit
                  (lambda ()
                    (compile-defs! items)
                    (let-values (((entries symbols?) (static-entries datums)))
                      (set! statics entries)
                      (let ((body (main-body forms (not (or (define-form? (car (last-pair all)))
                                                            (static-form? (car (last-pair all))))))))
                        (parameterize ((startup-defs `(,@(if symbols? (list (core-def 'intern-static-symbols!)) '())
                                                       ,library-init ,init)))
                          (compile-procedure
                           (make-def 'main "main" '() 'val #f #f #f #f #t #f 0 #f #f body env
                                     (make-main-core env body))
                           '())))))))
               (compile-literal-init init)
               ;; Before constant-entries, since it interns the names
               (let ((files (embedded-entries embedded)))
                 `((export "scheme_program")
                   (words "scheme_program" "scheme_main" "scheme_globals" ,(hashtable-size (top-globals))
                          "scheme_literals" ,(length (current-literals)) "scheme_embedded"
                          ,(if display? 1 0) "scheme_static_symbols" "scheme_embedded_data")
                   ,@files
                   ,@statics
                   ,@(global-entries (hashtable-size (top-globals)))
                   ,@(data-entries items)
                   ,@(literal-entries "scheme_literals" #f)
                   ,@(constant-entries)
                   ,@(reverse (current-entries))))))))))))
