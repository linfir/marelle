;; Compiling the library and programs, and the interface between them

(library (library)
  (export compile-scheme-library compile-scheme-program)
  (import (chezscheme)
          (utils)
          (parse)
          (front)
          (vars)
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
                              (and (memq ':native attrs) #t) (library-def-procedure? x)
                              (library-def-unsafe? x) 0 #f #f (library-def-body x) #f #f)))
          (when (and (def-native? def) (or (def-inline? def) (def-public? def)))
            (error 'scanned-item ":native with another attribute" name))
          ;; Natives take their arguments in the System V registers
          (when (and (def-native? def) (> (length (def-params def)) 6))
            (error 'scanned-item "too many parameters in a native def" name))
          ;; An emitted def is called with the Scheme convention
          (unless (or (not (def-emitted? def))
                      (def-native? def)
                      (and (for-all (lambda (k) (memq k '(val rest))) (def-params-kinds def))
                           (memq (def-result def) '(val never))))
            (error 'scanned-item "raw parameter or result in a non-inline def" name))
          (add-def-symbols! def)
          def)))


  ;; Each def's env: env, or unsafe-env for an unsafe def, then its core when it has a body
  (define (set-envs! items env unsafe-env)
    (for-each (lambda (x)
                (when (def? x)
                  (def-env-set! x (if (def-unsafe? x) unsafe-env env))
                  (unless (null? (def-body x))
                    (def-core-set! x (front-def (def-env x) (map car (def-params x))
                                                (def-body x))))))
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
    (if (memq (car entry) '(rodata function native))
        (list (cadr entry))
        '()))


  (define (print-strings)
    (for-each (lambda (s)
                (print-entry `(string ,(cdr s) ,(car s))))
              (reverse (current-strings))))


  (define (print-data items)
    (for-each (lambda (d)
                (print-entry `(data ,(data-symbol d) ,(data-nwords d) 0)))
              (filter (lambda (x) (and (data? x) (not (data-extern? x)))) items)))


  (define (new-compilation first-name thunk)
    (parameterize ((names (make-counter first-name))
                   (symbols (make-hashtable string-hash string=?))
                   (current-strings '())
                   (current-entries '())
                   (inlining '()))
      (thunk)))


  ;; Prints the IR of the library, its forms (the files of lib/), and returns the interface a
  ;; program compiles against: (names n), where a program's fresh names start, then each def,
  ;; inline ones with their body, and each data
  (define (compile-scheme-library forms)
    (new-compilation
     0
     (lambda ()
       (let ((items (map scanned-item (scan-library forms))))
         (let-values (((safe unsafe) (item-bindings items)))
           (set-envs! items safe (append safe unsafe unsafe-bindings)))
         (parameterize ((core-defs (map (lambda (x) (cons (item-name x) x)) (filter def? items))))
           (compile-defs! items))
         (let ((entries (reverse (current-entries))))
           (print-entry `(export ,@(apply append (map entry-symbols entries))
                                 ,@(map data-symbol (filter data? items))))
           (print-data items)
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
                               ,@(if (def-native? x) '(native) '()))
                              ,(def-unsafe? x) ,(def-procedure? x)
                              ,@(if (def-inline? x) (def-body x) '()))))
                  items))))))


  ;; The def or data of an interface entry, extern
  (define (interface-item x)
    (if (eq? (car x) 'data)
        (apply (lambda (name symbol nwords) (make-data name symbol nwords #t)) (cdr x))
        (apply (lambda (name symbol params result attrs unsafe? procedure? . body)
                 (make-def name symbol params result (and (memq 'inline attrs) #t)
                           (and (memq 'public attrs) #t) (and (memq 'native attrs) #t)
                           procedure? unsafe? 0 #f #t body #f #f))
               (cdr x))))


  (define (define-data-form? form)
    (and (non-empty-list? form) (eq? (car form) 'define-data)))


  (define (splice-begins forms)
    (apply append (map (lambda (x)
                         (if (and (non-empty-list? x) (eq? (car x) 'begin))
                             (splice-begins (cdr x))
                             (list x)))
                       forms)))


  ;; The body of main: forms in order, each define a set! of its global, then undef if the
  ;; program ends with a define
  (define (main-body forms globals ends-with-define?)
    (let loop ((forms forms)
               (globals globals)
               (acc '()))
      (cond
       ((null? forms)
        (reverse (if ends-with-define? (cons undef acc) acc)))
       ((define-form? (car forms))
        (let ((g (car globals)))
          (loop (cdr forms)
                (cdr globals)
                (cons `(,(marker 'set!) ,(global-def-name g)
                        ,(if (global-def-args g)
                             `(,(marker 'lambda) ,(global-def-args g) ,@(global-def-body g))
                             (car (global-def-body g))))
                      acc))))
       (else
        (loop (cdr forms) globals (cons (car forms) acc))))))


  ;; Prints the IR of the program, its top-level forms with begins spliced: a define with a declare
  ;; and define-data are static, like the library's, other defines are global cells, set in order
  ;; by main among the expressions; the last form gives the result
  (define (compile-scheme-program interface program)
    (when (null? (splice-begins program))
      (error 'compile-scheme-program "empty program"))
    (new-compilation
     (cadr (assq 'names interface))
     (lambda ()
       (let ((library (map interface-item
                           (filter (lambda (x) (memq (car x) '(def data))) interface))))
         (add-symbol! "main_proc" 'main)
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
                       ((globals) (scan-defs (filter define-form? forms) make-global-def))
                       ((items) (map scanned-item (scan-library static)))
                       ((safe unsafe) (item-bindings items)))
           (for-each (lambda (g)
                       (when (find (lambda (x) (eq? (item-name x) (global-def-name g))) items)
                         (error 'compile-scheme-program "redefinition not allowed" (global-def-name g))))
                     globals)
           (let* ((env (append (map (lambda (g)
                                      (cons (global-def-name g)
                                            (make-global-binding (global-def-index g))))
                                    globals)
                               safe
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
               (let ((body (main-body forms globals (or (define-form? (car (last-pair all)))
                                                        (define-data-form? (car (last-pair all)))))))
                 (compile-procedure
                  (make-def 'main "main" '() 'val #f #f #f #t #f 0 #f #f body env
                            (front-def env '() body))
                  env '())))
             (print-entry '(export "scheme_main" "scheme_globals" "scheme_nglobals"))
             (emit-globals (length globals))
             (print-data items)
             (print-strings)
             (for-each print-entry (reverse (current-entries))))))))))
