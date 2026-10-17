(library (low-parse)
  (export parse-program binary-ops tests keywords bindings?
          def-name def-params def-result def-inline? def-public? def-body def-emitted? def-as
          def-anonymous? def-captures def-native? def-extern?
          def-proc def-code def-params-names def-params-kinds
          data-nwords data-symbol data-extern?)
  (import (chezscheme)
          (utils))


  ;; params   (symbol . kind) pairs
  ;; result   kind
  ;; emitted? #f when the def is only inlined
  ;; as       name Scheme sees, from (:as name), else the def name
  ;; anonymous? from :anonymous, the procedure object has no name
  ;; captures number of captures from (:captures n), 0 for a static procedure
  ;; native?  from :native, System V convention and a machine stack frame, never collects
  ;; extern?  from :extern, emitted in another object (separate compilation)
  ;; symbol   the mangled name, prefix of the def's symbols
  (define-record-type def
    (fields name params result inline? public? body emitted? as anonymous? captures native?
            extern? symbol))


  ;; nwords   number of words, zeroed
  ;; extern?  from (:extern), defined in another object
  ;; symbol   the mangled name
  (define-record-type data
    (fields name nwords extern? symbol))


  (define (def-proc def) (string-append (def-symbol def) "_proc"))
  (define (def-code def)
    (if (eq? (def-name def) 'main)
        "scheme_main"
        (string-append (def-symbol def) "_code")))
  (define (def-params-names def) (map car (def-params def)))
  (define (def-params-kinds def) (map cdr (def-params def)))


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


  (define keywords
    (append (map car binary-ops)
            (map car tests)
            '(if when unless let let* begin set!
                 and or not
                 raw as-raw as-val
                 syscall quote undef
                 load store load8 store8 copy-words fill-words alloc self address)))


  ;; Per program, set by parse-program
  (define name-kinds (make-parameter #f))
  ;; symbol => the name it was made from
  (define symbols (make-parameter #f))


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


  ;; (key x) in attrs => x, else #f
  (define (attr-value attrs key)
    (let ((a (find (lambda (a) (and (pair? a) (eq? (car a) key))) attrs)))
      (and a (cadr a))))


  ;; (val x) => (x . val)
  (define (parse-param x)
    (unless (and (list? x)
                 (= (length x) 2)
                 (memq (car x) '(val raw))
                 (symbol? (cadr x)))
      (error 'parse-param "invalid parameter" x))
    (cons (cadr x) (car x)))


  (define (parse-result x)
    (cond
     ((null? x) 'void)
     ((eq? x '!) 'never)
     ((and (list? x) (= (length x) 1) (memq (car x) '(val raw))) (car x))
     (else (error 'parse-result "invalid result" x))))


  ;; Bind x as a def or a local: a def name is never reused,
  ;; and a local name only after the local that had it went out of scope (free)
  (define (bind-name! x kind)
    (let ((old (hashtable-ref (name-kinds) x #f)))
      (cond
       ((memq x keywords)
        (error 'bind-name!
               (if (eq? kind 'def) "def named like a keyword" "local variable named like a keyword")
               x))
       ((not old))
       ((eq? kind 'def)
        (error 'bind-name! (if (eq? old 'def) "name already defined" "def named like a local") x))
       ((eq? old 'def)
        (error 'bind-name! "local variable named like a def" x))
       ((eq? old 'local)
        (error 'bind-name! "shadowed variable" x)))
      (hashtable-set! (name-kinds) x kind)))


  (define (unbind-local! x)
    (hashtable-set! (name-kinds) x 'free))


  ;; The bindings of a let: ((name expr) ...)
  (define (bindings? x)
    (and (list? x)
         (for-all (lambda (b)
                    (and (list? b)
                         (= (length b) 2) (symbol? (car b))))
                  x)))


  ;; Bind the params, then the variables of each let in body while it is in scope;
  ;; malformed forms are scanned as plain lists, the compiler rejects them later
  (define (bind-locals! params body)
    (define (walk x)
      (cond
       ((not (non-empty-list? x)))
       ((eq? (car x) 'quote))
       ;; (let name ((var init) ...) body ...)
       ((and (eq? (car x) 'let)
             (>= (length x) 4)
             (symbol? (cadr x))
             (bindings? (caddr x)))
        (let ((names (cons (cadr x) (map car (caddr x)))))
          (for-each (lambda (b) (walk (cadr b))) (caddr x))
          (for-each (lambda (v) (bind-name! v 'local)) names)
          (for-each walk (cdddr x))
          (for-each unbind-local! names)))
       ((and (memq (car x) '(let let*))
             (>= (length x) 3)
             (bindings? (cadr x)))
        (let ((star? (eq? (car x) 'let*))
              (vars (map car (cadr x))))
          (for-each (lambda (b)
                      (walk (cadr b))
                      (when star?
                        (bind-name! (car b) 'local)))
                    (cadr x))
          (unless star?
            (for-each (lambda (v) (bind-name! v 'local)) vars))
          (for-each walk (cddr x))
          (for-each unbind-local! vars)))
       (else (for-each walk x))))
    (for-each (lambda (p) (bind-name! p 'local)) params)
    (for-each walk body)
    (for-each unbind-local! params))


  ;; (def name (param ...) -> (kind ...) (attr ...) body ...);
  ;; an :extern def has no body unless it is :inline
  (define (parse-def form)
    (unless (and (list? form)
                 (>= (length form) 6)
                 (eq? (car form) 'def)
                 (symbol? (cadr form))
                 (list? (caddr form))
                 (eq? (cadddr form) '->)
                 (list? (list-ref form 5)))
      (error 'parse-def "invalid def" form))
    (let* ((attrs (list-ref form 5))
           (body (list-tail form 6))
           (name (cadr form))
           (inline? (and (memq ':inline attrs) #t))
           (public? (and (memq ':public attrs) #t))
           (anonymous? (and (memq ':anonymous attrs) #t))
           (native? (and (memq ':native attrs) #t))
           (extern? (and (memq ':extern attrs) #t))
           (as (attr-value attrs ':as))
           (captures (or (attr-value attrs ':captures) 0)))
      (unless (eq? (null? body) (and extern? (not inline?)))
        (error 'parse-def (if (null? body) "def without a body" "body in an extern def") name))
      (for-each (lambda (a)
                  (unless (or (memq a '(:inline :public :anonymous :native :extern))
                              (and (pair? a) (= (length a) 2)
                                   (or (and (eq? (car a) ':as) (symbol? (cadr a)))
                                       (and (eq? (car a) ':captures)
                                            (integer? (cadr a)) (exact? (cadr a))
                                            (>= (cadr a) 0)))))
                    (error 'parse-def "unknown attribute" a)))
                attrs)
      (when (and as anonymous?)
        (error 'parse-def ":as and :anonymous" name))
      (when (and (> captures 0) (or inline? public? (eq? name 'main)))
        (error 'parse-def ":captures requires a non-inline, non-public def other than main" name))
      (when (and native? (or inline? public? anonymous? as (> captures 0) (eq? name 'main)))
        (error 'parse-def ":native with another attribute" name))
      (when (and (eq? name 'main)
                 (or inline? (not (null? (caddr form))) (not (equal? (list-ref form 4) '(val)))))
        (error 'parse-def "main must be a non-inline def with no parameters and a val result" form))
      (let ((params (map parse-param (caddr form))))
        (bind-name! name 'def)
        (bind-locals! (map car params) body)
        (let ((result (parse-result (list-ref form 4)))
              (emitted? (or public? (not inline?))))
          ;; An emitted def is called with the Scheme convention
          (unless (or (not emitted?)
                      native?
                      (and (for-all (lambda (p) (eq? (cdr p) 'val)) params)
                           (memq result '(val never))))
            (error 'parse-def "raw parameter or result in a non-inline def" name))
          (let ((def (make-def name params result inline? public? body emitted?
                               (or as name) anonymous? captures native? extern? (mangle name))))
            (for-each (lambda (sym) (add-symbol! sym name))
                      (if (eq? name 'main)
                          (list (def-proc def))
                          (list (def-code def) (def-proc def))))
            def)))))


  ;; (globals n): the number of global cells; seen is the count already given, or #f
  (define (parse-globals form seen)
    (unless (and (= (length form) 2)
                 (integer? (cadr form))
                 (exact? (cadr form))
                 (>= (cadr form) 0))
      (error 'parse-globals "invalid globals" form))
    (when seen
      (error 'parse-globals "globals already defined" form))
    (cadr form))


  ;; (data name nwords (attr ...)): static words outside the heap
  (define (parse-data form)
    (unless (and (list? form)
                 (= (length form) 4)
                 (symbol? (cadr form))
                 (integer? (caddr form))
                 (exact? (caddr form))
                 (> (caddr form) 0)
                 (list? (cadddr form)))
      (error 'parse-data "invalid data" form))
    (let ((name (cadr form))
          (attrs (cadddr form)))
      (for-each (lambda (a)
                  (unless (eq? a ':extern)
                    (error 'parse-data "unknown attribute" a)))
                attrs)
      (bind-name! name 'def)
      (let ((sym (mangle name)))
        (add-symbol! sym name)
        (make-data name (caddr form) (and (memq ':extern attrs) #t) sym))))


  ;; The (name . def) and (name . data) alists in source order, and the number of global cells
  (define (parse-program forms)
    (parameterize ((name-kinds (make-eq-hashtable))
                   (symbols (make-hashtable string-hash string=?)))
      (let loop ((forms forms)
                 (defs '())
                 (datas '())
                 (globals #f))
        (if (null? forms)
            (values (reverse defs) (reverse datas) (or globals 0))
            (let ((form (car forms)))
              (case (and (pair? form) (car form))
                ((def)
                 (let ((def (parse-def form)))
                   (loop (cdr forms) (acons (def-name def) def defs) datas globals)))
                ((data)
                 (let ((d (parse-data form)))
                   (loop (cdr forms) defs (acons (data-name d) d datas) globals)))
                ((globals)
                 (loop (cdr forms) defs datas (parse-globals form globals)))
                (else
                 (error 'low "unknown top-level form" form)))))))))
