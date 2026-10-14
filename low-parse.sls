(library (low-parse)
  (export parse-program binary-ops tests keywords bindings?
          def-name def-params def-result def-inline? def-public? def-body def-n def-as
          def-anonymous? def-captures
          def-proc def-code def-params-names def-params-kinds)
  (import (chezscheme)
          (utils))


  ;; params   (symbol . kind) pairs
  ;; result   kind
  ;; n        number of the emitted function, #f when the def is only inlined
  ;; as       name Scheme sees, from (:as name), else the def name
  ;; anonymous? from :anonymous, the procedure object has no name
  ;; captures number of captures from (:captures n), 0 for a static procedure
  (define-record-type def
    (fields name params result inline? public? body n as anonymous? captures))


  (define (def-proc def) (format "proc~a" (def-n def)))
  (define (def-code def)
    (if (eq? (def-name def) 'main)
        "scheme_main"
        (format "fn~a" (def-n def))))
  (define (def-params-names def) (map car (def-params def)))
  (define (def-params-kinds def) (map cdr (def-params def)))


  (define binary-ops
    '((bit-and . and)
      (bit-or . or)
      (raw+ . add)
      (raw- . sub)
      (raw* . mul)
      (raw<< . sal)
      (raw>> . sar)))


  ;; (test jump-if-true jump-if-false)
  (define tests
    '((raw=  je  jne)
      (raw<  jl  jge)
      (raw<= jle jg)
      (raw>  jg  jle)
      (raw>= jge jl)))


  (define keywords
    (append (map car binary-ops)
            (map car tests)
            '(if when unless let let* begin set!
              and or not
              raw as-raw as-val
              c-call quote
              load store alloc self address)))


  (define fn-counter (make-counter))
  (define name-kinds (make-eq-hashtable))


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
    (let ((old (hashtable-ref name-kinds x #f)))
      (cond
       ((not old))
       ((eq? kind 'def)
        (error 'bind-name! (if (eq? old 'def) "name already defined" "def named like a local") x))
       ((eq? old 'def)
        (error 'bind-name! "local variable named like a def" x))
       ((eq? old 'local)
        (error 'bind-name! "shadowed variable" x)))
      (hashtable-set! name-kinds x kind)))


  (define (unbind-local! x)
    (hashtable-set! name-kinds x 'free))


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
       ((not (list? x)))
       ((null? x))
       ((eq? (car x) 'quote))
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
           (as (attr-value attrs ':as))
           (captures (or (attr-value attrs ':captures) 0)))
      (for-each (lambda (a)
                  (unless (or (memq a '(:inline :public :anonymous))
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
      (when (memq name keywords)
        (error 'parse-def "name already defined" name))
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
                      (and (for-all (lambda (p) (eq? (cdr p) 'val)) params)
                           (eq? result 'val)))
            (error 'parse-def "raw parameter or result in a non-inline def" name))
          (make-def name params result inline? public? body
                    (and emitted? (fn-counter))
                    (or as name) anonymous? captures)))))


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


  ;; The (name . def) alist in source order, and the number of global cells
  (define (parse-program forms)
    (let loop ((forms forms)
               (defs '())
               (globals #f))
      (if (null? forms)
          (values (reverse defs) (or globals 0))
          (let ((form (car forms)))
            (case (and (pair? form) (car form))
              ((def)
               (let ((def (parse-def form)))
                 (loop (cdr forms) (acons (def-name def) def defs) globals)))
              ((globals)
               (loop (cdr forms) defs (parse-globals form globals)))
              (else
               (error 'low "unknown top-level form" form))))))))
