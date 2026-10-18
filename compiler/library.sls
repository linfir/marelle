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
          (defs)
          (compile)
          (output))


  (define (item-name x)
    (if (def? x) (def-name x) (data-name x)))


  (define (item-binding x)
    (if (def? x) (make-def-binding x) (make-data-binding x)))


  ;; Data, and defs with raw parameters or result, only unsafe defines may use
  (define (unsafe-item? x)
    (or (data? x)
        (eq? (def-result x) 'raw)
        (memq 'raw (def-params-kinds x))
        #f))


  ;; The bindings of the safe items, then those of the unsafe ones
  (define (item-bindings items)
    (values (map (lambda (x) (cons (item-name x) (item-binding x)))
                 (filter (lambda (x) (not (unsafe-item? x))) items))
            (map (lambda (x) (cons (item-name x) (item-binding x)))
                 (filter unsafe-item? items))))


  ;; The def or data of a library define or define-data; a procedure gets a fresh symbol,
  ;; since programs may have their own of the same name
  (define (scanned-item x)
    (if (library-data? x)
        (let ((d (make-data (library-data-name x) (mangle (library-data-name x))
                            (library-data-nwords x) #f)))
          (add-symbol! (data-symbol d) (data-name d))
          d)
        (let* ((name (library-def-name x))
               (attrs (library-def-attrs x))
               (def (make-def name
                              (mangle (if (library-def-procedure? x) (fresh-name name) name))
                              (map cons (library-def-args x) (library-def-kinds x))
                              (library-def-result x)
                              (and (memq ':inline attrs) #t) (and (memq ':public attrs) #t)
                              (and (memq ':noalloc attrs) #t) (library-def-procedure? x)
                              (library-def-unsafe? x) 0 #f #f (library-def-body x) #f #f)))
          (when (and (def-noalloc? def) (or (def-inline? def) (def-public? def)))
            (error 'scanned-item ":noalloc with another attribute" name))
          ;; An emitted def is called with the Scheme convention
          (unless (or (not (def-emitted? def))
                      (def-noalloc? def)
                      (and (for-all (lambda (k) (memq k '(val rest))) (def-params-kinds def))
                           (memq (def-result def) '(val never))))
            (error 'scanned-item "raw parameter or result in a non-inline def" name))
          (add-def-symbols! def)
          def)))


  ;; The library defs a letrec init may call and stay simple
  (define pure-names
    '(car cdr cons + - * = < > <= >= eq? eqv? not null? pair? boolean? integer? procedure?
      char? string? symbol? vector? zero?))


  ;; Per compilation: the library's defs of pure-names
  (define pure-defs (make-parameter '()))


  (define (set-pure-defs! library)
    (pure-defs (filter (lambda (x) (and (def? x) (memq (def-name x) pure-names))) library)))


  (define (pure-def-name? env x)
    (let ((entry (assq x env)))
      (and entry
           (def-binding? (cdr entry))
           (memq (def-binding-def (cdr entry)) (pure-defs))
           #t)))


  ;; The core of a def, its letrec* forms fixed, analyzed, its closures converted
  (define (make-core env names body unsafe?)
    (let* ((core (front-def env names body unsafe?))
           (core (cons (car core) (fix-letrec (cdr core) (lambda (x) (pure-def-name? env x))))))
      (analyze-def! (car core) (cdr core))
      (cons (car core) (convert-closures (cdr core)))))


  ;; The core of main, whose body is a letrec*: its variables not bound by a fix get global
  ;; cells
  (define (make-main-core env body)
    (let* ((core (front-def env '() body #f))
           (vars (map car (cadr (cadr core))))
           (core (cons '() (fix-letrec (cdr core) (lambda (x) (pure-def-name? env x)))))
           (fixed (fix-names core)))
      (for-each (lambda (x)
                  (unless (memq x fixed)
                    (hashtable-set! (top-globals) x (hashtable-size (top-globals)))))
                vars)
      (analyze-def! '() (cdr core))
      (cons '() (convert-closures (cdr core)))))


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
                    (def-core-set! x (make-core (def-env x) (map car (def-params x)) (def-body x)
                                                (def-unsafe? x))))))
              items))


  ;; The emitted defs that are not extern, compiled in their env
  (define (compile-defs! items)
    (for-each (lambda (x)
                (when (and (def? x) (def-emitted? x) (not (def-extern? x)))
                  (if (def-procedure? x)
                      (compile-procedure x (def-env x) '())
                      (compile-low-def x))))
              items))


  ;; The symbols an entry defines
  (define (entry-symbols entry)
    (if (memq (car entry) '(rodata function noalloc))
        (list (cadr entry))
        '()))


  (define (print-strings)
    (for-each (lambda (s)
                (print-entry `(string ,(cdr s) ,(car s))))
              (reverse (current-strings))))


  ;; The cells of the literals, 0 until the init function fills them, and their count
  (define (print-literals symbol count-symbol)
    (let ((n (length (current-literals))))
      (print-entry `(data ,symbol ,n 0))
      (print-entry `(rodata ,count-symbol (quad ,n)))))


  ;; The def of the function filling a unit's literal cells, called by main
  (define (literal-init-def symbol extern?)
    (make-def 'literal-init symbol '() 'val #f #f #f #f #t 0 #t extern? '() #f #f))


  (define (print-data items)
    (for-each (lambda (d)
                (print-entry `(data ,(data-symbol d) ,(data-nwords d) 0)))
              (filter (lambda (x) (and (data? x) (not (data-extern? x)))) items)))


  (define (new-compilation first-name literals thunk)
    (parameterize ((names (make-counter first-name))
                   (symbols (make-hashtable string-hash string=?))
                   (current-strings '())
                   (current-literals '())
                   (literals-symbol literals)
                   (startup-defs '())
                   (current-entries '())
                   (inlining '())
                   (var-facts (make-eq-hashtable))
                   (lambda-frees (make-eq-hashtable))
                   (top-globals (make-eq-hashtable))
                   (pure-defs '()))
      (thunk)))


  ;; Prints the IR of the library, its forms (the files of lib/), and returns the interface a
  ;; program compiles against: (names n), where a program's fresh names start, then each def,
  ;; inline ones with their body, and each data
  (define (compile-scheme-library forms)
    (new-compilation
     0 "scheme_library_literals"
     (lambda ()
       (let ((items (map scanned-item (scan-library forms)))
             (init (literal-init-def "scheme_library_init" #f)))
         (add-def-symbols! init)
         (set-pure-defs! items)
         (let-values (((safe unsafe) (item-bindings items)))
           (set-envs! items safe (append safe unsafe unsafe-bindings)))
         (parameterize ((core-defs (map (lambda (x) (cons (item-name x) x)) (filter def? items))))
           (compile-defs! items)
           (compile-literal-init init))
         (let ((entries (reverse (current-entries))))
           (print-entry `(export ,@(apply append (map entry-symbols entries))
                                 ,@(map data-symbol (filter data? items))))
           (print-data items)
           (print-literals "scheme_library_literals" "scheme_library_nliterals")
           (print-strings)
           (for-each print-entry entries)
           (print-entry '(stubs)))
         `((names ,((names)))
           ,@(map (lambda (x)
                    (if (data? x)
                        `(data ,(data-name x) ,(data-symbol x) ,(data-nwords x))
                        `(def ,(def-name x) ,(def-symbol x) ,(def-params x) ,(def-result x)
                              (,@(if (def-inline? x) '(inline) '())
                               ,@(if (def-public? x) '(public) '())
                               ,@(if (def-noalloc? x) '(noalloc) '()))
                              ,(def-unsafe? x) ,(def-procedure? x)
                              ,@(if (def-inline? x) (def-body x) '()))))
                  items))))))


  ;; The def or data of an interface entry, extern
  (define (interface-item x)
    (if (eq? (car x) 'data)
        (apply (lambda (name symbol nwords) (make-data name symbol nwords #t)) (cdr x))
        (apply (lambda (name symbol params result attrs unsafe? procedure? . body)
                 (make-def name symbol params result (and (memq 'inline attrs) #t)
                           (and (memq 'public attrs) #t) (and (memq 'noalloc attrs) #t)
                           procedure? unsafe? 0 #f #t body #f #f))
               (cdr x))))


  (define (define-data-form? form)
    (and (non-empty-list? form) (eq? (car form) 'define-data)))


  ;; Also expands define-values
  (define (splice-begins forms)
    (apply append (map (lambda (x)
                         (cond
                          ((and (non-empty-list? x) (eq? (car x) 'begin))
                           (splice-begins (cdr x)))
                          ((define-values-form? x)
                           (expand-top-define-values x))
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
            (error 'compile-scheme-program "redefinition not allowed" name))
          (loop (cdr forms)
                (cons (list name (if args `(,(marker 'lambda) ,args ,@body) (car body)))
                      bindings))))
       (else
        (loop (cdr forms) (cons (list (gensym "expression") (car forms)) bindings))))))


  ;; Prints the IR of the program, its top-level forms with begins spliced: a define with a declare
  ;; and define-data are static, like the library's; the others are the letrec* body of main
  ;; (main-body), whose variables, but the procedures, are global cells; the last form gives the
  ;; result
  (define (compile-scheme-program interface program)
    (when (null? (splice-begins program))
      (error 'compile-scheme-program "empty program"))
    (new-compilation
     (cadr (assq 'names interface)) "scheme_literals"
     (lambda ()
       (let ((library (map interface-item
                           (filter (lambda (x) (memq (car x) '(def data))) interface)))
             (library-init (literal-init-def "scheme_library_init" #t))
             (init (literal-init-def "scheme_init" #f)))
         (add-symbol! "main_proc" 'main)
         (add-def-symbols! library-init)
         (add-def-symbols! init)
         (set-pure-defs! library)
         (for-each (lambda (x)
                     (if (def? x)
                         (add-def-symbols! x)
                         (add-symbol! (data-symbol x) (data-name x))))
                   library)
         (let-values (((safe unsafe) (item-bindings library)))
           (set-envs! library safe (append safe unsafe unsafe-bindings)))
         (let*-values (((all) (splice-begins program))
                       ((static) (filter (lambda (x) (or (declared-define? x) (define-data-form? x)))
                                         all))
                       ((forms) (filter (lambda (x) (not (memq x static))) all))
                       ((items) (map scanned-item (scan-library static)))
                       ((safe unsafe) (item-bindings items)))
           (for-each (lambda (x)
                       (let-values (((name args body) (parse-define x)))
                         (when (find (lambda (x) (eq? (item-name x) name)) items)
                           (error 'compile-scheme-program "redefinition not allowed" name))))
                     (filter define-form? forms))
           (let* ((env (append safe
                               (map (lambda (x) (cons (def-name x) (make-def-binding x)))
                                    (filter (lambda (x) (and (def? x) (def-public? x))) library))))
                  (unsafe-env (append env unsafe
                                      (map (lambda (x) (cons (item-name x) (item-binding x)))
                                           library)
                                      unsafe-bindings)))
             (set-envs! items env unsafe-env)
             (parameterize ((core-defs (map (lambda (x) (cons (def-name x) x))
                                            (filter def? library))))
               (compile-defs! items)
               (let ((body (main-body forms (not (or (define-form? (car (last-pair all)))
                                                     (define-data-form? (car (last-pair all))))))))
                 (parameterize ((startup-defs (list library-init init)))
                   (compile-procedure
                    (make-def 'main "main" '() 'val #f #f #f #t #f 0 #f #f body env
                              (make-main-core env body))
                    env '())))
               (compile-literal-init init))
             (print-entry '(export "scheme_main" "scheme_globals" "scheme_nglobals"
                                   "scheme_literals" "scheme_nliterals"))
             (emit-globals (hashtable-size (top-globals)))
             (print-data items)
             (print-literals "scheme_literals" "scheme_nliterals")
             (print-strings)
             (for-each print-entry (reverse (current-entries))))))))))
