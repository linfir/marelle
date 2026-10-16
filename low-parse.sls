(library (low-parse)
  (export parse-program binary-ops tests keywords bindings?
          def-name def-params def-result def-inline? def-public? def-body def-n def-as
          def-anonymous? def-captures def-native? def-c-name
          def-proc def-code def-params-names def-params-kinds
          data-nwords data-symbol data-export?)
  (import (chezscheme)
          (utils))


  ;; params   (symbol . kind) pairs
  ;; result   kind
  ;; n        number of the emitted function, #f when the def is only inlined
  ;; as       name Scheme sees, from (:as name), else the def name
  ;; anonymous? from :anonymous, the procedure object has no name
  ;; captures number of captures from (:captures n), 0 for a static procedure
  ;; native?  from :native, System V convention and a machine stack frame, never collects
  ;; c-name   from (:c-name "name"), the exported symbol of a native def, else #f
  (define-record-type def
    (fields name params result inline? public? body n as anonymous? captures native? c-name))


  ;; nwords   number of words, zeroed
  ;; symbol   from (:c-name "name"), else a local label
  ;; export?  from (:c-name "name")
  (define-record-type data
    (fields name nwords symbol export?))


  (define (def-proc def) (format "proc~a" (def-n def)))
  (define (def-code def)
    (cond
     ((def-c-name def))
     ((eq? (def-name def) 'main) "scheme_main")
     (else (format "fn~a" (def-n def)))))
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
      (raw<< . sal)
      (raw>> . sar)))


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
  (define fn-counter (make-parameter #f))
  (define data-counter (make-parameter #f))
  (define name-kinds (make-parameter #f))


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


  ;; (def name (param ...) -> (kind ...) (attr ...) body ...)
  (define (parse-def form)
    (unless (and (list? form)
                 (>= (length form) 7)
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
           (c-name (attr-value attrs ':c-name))
           (as (attr-value attrs ':as))
           (captures (or (attr-value attrs ':captures) 0)))
      (for-each (lambda (a)
                  (unless (or (memq a '(:inline :public :anonymous :native))
                              (and (pair? a) (= (length a) 2)
                                   (or (and (eq? (car a) ':as) (symbol? (cadr a)))
                                       (and (eq? (car a) ':c-name) (string? (cadr a)))
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
      (when (and c-name (not native?))
        (error 'parse-def ":c-name requires :native" name))
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
          (make-def name params result inline? public? body
                    (and emitted? ((fn-counter)))
                    (or as name) anonymous? captures native? c-name)))))


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
                  (unless (and (pair? a) (= (length a) 2)
                               (eq? (car a) ':c-name) (string? (cadr a)))
                    (error 'parse-data "unknown attribute" a)))
                attrs)
      (bind-name! name 'def)
      (let ((c-name (attr-value attrs ':c-name)))
        (make-data name (caddr form)
                   (or c-name (format "data~a" ((data-counter))))
                   (and c-name #t)))))


  ;; The (name . def) and (name . data) alists in source order, and the number of global cells
  (define (parse-program forms)
    (parameterize ((fn-counter (make-counter))
                   (data-counter (make-counter))
                   (name-kinds (make-eq-hashtable)))
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
