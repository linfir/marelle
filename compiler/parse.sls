(library (parse)
  (export undef unbound immediate? vector-literal? special? special-keyword source-keyword marker
          define-form? validate-parameters
          formals-names formals->params
          parse-lambda parse-let parse-let* parse-letrec named-let? parse-named-let
          parse-if parse-cond parse-case parse-do parse-begin parse-set! parse-define
          parse-let-values parse-define-values define-values-form? core-ref core-ref-name
          scan-defs scan-library
          make-global-def global-def-name global-def-index global-def-args global-def-body
          library-def? library-def-name library-def-args library-def-kinds
          library-def-result library-def-attrs library-def-body library-def-procedure?
          library-def-unsafe? declared-define? parse-library-define parse-define-data
          library-data? library-data-name library-data-nwords)
  (import (chezscheme)
          (utils))


  (define int-bits 61)
  (define int-min (- (expt 2 (- int-bits 1))))
  (define int-max (- (expt 2 (- int-bits 1)) 1))


  (define undef (void))


  ;; The value of a letrec variable before its init, which no program can make
  (define-record-type unbound-value)
  (define unbound (make-unbound-value))


  (define (immediate? x)
    (or (and (integer? x)
             (exact? x)
             (<= int-min x int-max))
        (boolean? x)
        (char? x)
        (equal? x ''())
        (eq? x undef)
        (eq? x unbound)))


  ;; #(...) or '#(...)
  (define (vector-literal? x)
    (or (vector? x)
        (and (list? x) (= (length x) 2) (eq? (car x) 'quote) (vector? (cadr x)))))


  (define *special-forms*
    '(if cond case and or let let* letrec letrec* lambda begin set! when unless do define
      let-values let*-values define-values))


  ;; Heads of the forms the compiler builds, so a program rebinding if or let keeps them;
  ;; fix, check-defined, closure and core have no source form
  (define markers
    (map (lambda (k) (cons (gensym (symbol->string k)) k))
         (append '(eq? fix check-defined closure core) *special-forms*)))


  (define (marker keyword)
    (car (find (lambda (m) (eq? (cdr m) keyword)) markers)))


;; (core name), the library def name, even where a program rebinds it
  (define (core-ref name)
    `(,(marker 'core) ,name))


  ;; The name of the (core name) form x, or #f
  (define (core-ref-name x)
    (and (pair? x) (eq? (special-keyword x) 'core) (cadr x)))


  ;; In source, the special form expr is, by its keyword, or #f
  (define (source-keyword env expr)
    (and (non-empty-list? expr)
         (let ((x (car expr)))
           (cond
            ((assq x markers) => cdr)
            ((and (symbol? x)
                  (memq x *special-forms*)
                  (not (assq x env)))
             x)
            (else #f)))))


  ;; After the front pass, the special form expr is, by its marker, or #f
  (define (special-keyword expr)
    (and (pair? expr)
         (cond
          ((assq (car expr) markers) => cdr)
          (else #f))))


  (define (special? expr)
    (and (special-keyword expr) #t))


  ;; Formals are (x ...), (x ... . rest) or rest
  (define (formals-names formals)
    (cond
     ((null? formals) '())
     ((pair? formals) (cons (car formals) (formals-names (cdr formals))))
     (else (list formals))))


  ;; ((x . val) ... (rest . rest)): a rest parameter holds the list of the extra arguments
  (define (formals->params formals)
    (cond
     ((null? formals) '())
     ((pair? formals) (cons (cons (car formals) 'val) (formals->params (cdr formals))))
     (else (list (cons formals 'rest)))))


  (define (validate-parameters formals)
    (let loop ((args (formals-names formals))
               (seen '()))
      (unless (null? args)
        (let ((arg (car args)))
          (unless (symbol? arg)
            (error 'validate-parameters "invalid parameter" arg))
          (when (memq arg seen)
            (error 'validate-parameters "duplicate parameter" arg))
          (loop (cdr args) (cons arg seen))))))


  ;; (lambda formals body ...) => (values formals body)
  (define (parse-lambda form)
    (unless (>= (length form) 3)
      (error 'parse-lambda "invalid lambda" form))
    (validate-parameters (cadr form))
    (values (cadr form) (cddr form)))


  ;; (let ((var init) ...) body ...) => (values bindings body)
  (define (parse-let form)
    (parse-bindings 'parse-let form #f))


  (define (named-let? form)
    (and (>= (length form) 4)
         (symbol? (cadr form))))


  ;; (let name ((var init) ...) body ...) => (values name bindings body)
  (define (parse-named-let form)
    (let-values (((bindings body) (parse-let (cdr form))))
      (values (cadr form) bindings body)))


  ;; (let* ((var init) ...) body ...) => (values bindings body)
  (define (parse-let* form)
    (parse-bindings 'parse-let* form #t))


  ;; (letrec ((var init) ...) body ...), or letrec* => (values bindings body)
  (define (parse-letrec form)
    (parse-bindings 'parse-letrec form #f))


  (define (parse-bindings who form duplicates-ok?)
    (unless (and (>= (length form) 3)
                 (list? (cadr form)))
      (error who "invalid let" form))
    (let loop ((bs (cadr form))
               (seen '()))
      (unless (null? bs)
        (let ((b (car bs)))
          (unless (and (list? b)
                       (= (length b) 2)
                       (symbol? (car b)))
            (error who "invalid let binding" b))
          (when (and (not duplicates-ok?) (memq (car b) seen))
            (error who "duplicate let variable" (car b)))
          (loop (cdr bs) (cons (car b) seen)))))
    (values (cadr form) (cddr form)))


;; (let-values ((formals init) ...) body ...), or let*-values => (values bindings body)
  (define (parse-let-values form duplicates-ok?)
    (unless (and (>= (length form) 3)
                 (list? (cadr form)))
      (error 'parse-let-values "invalid let-values" form))
    (let loop ((bs (cadr form))
               (seen '()))
      (unless (null? bs)
        (let ((b (car bs)))
          (unless (and (list? b) (= (length b) 2))
            (error 'parse-let-values "invalid let-values binding" b))
          (validate-parameters (car b))
          (let ((names (formals-names (car b))))
            (unless duplicates-ok?
              (for-each (lambda (x)
                          (when (memq x seen)
                            (error 'parse-let-values "duplicate let-values variable" x)))
                        names))
            (loop (cdr bs) (append names seen))))))
    (values (cadr form) (cddr form)))


  (define (define-values-form? form)
    (and (non-empty-list? form)
         (eq? (car form) 'define-values)))


  ;; (define-values formals expr) => (values formals expr)
  (define (parse-define-values form)
    (unless (= (length form) 3)
      (error 'parse-define-values "invalid define-values" form))
    (validate-parameters (cadr form))
    (values (cadr form) (caddr form)))


  ;; (if test then [else]) => (values test then else)
  (define (parse-if form)
    (unless (<= 3 (length form) 4)
      (error 'parse-if "invalid if" form))
    (values (cadr form)
            (caddr form)
            (if (null? (cdddr form)) undef (cadddr form))))


  ;; (cond clause ...) => (clause ...); else and => are checked by the caller
  (define (parse-cond form)
    (for-each (lambda (c)
                (unless (non-empty-list? c)
                  (error 'parse-cond "invalid cond clause" c)))
              (cdr form))
    (cdr form))


  ;; (case key clause ...) => (values key clauses); else and => are checked by the caller
  (define (parse-case form)
    (unless (>= (length form) 2)
      (error 'parse-case "invalid case" form))
    (for-each (lambda (c)
                (unless (and (non-empty-list? c) (pair? (cdr c)))
                  (error 'parse-case "invalid case clause" c)))
              (cddr form))
    (values (cadr form) (cddr form)))


  ;; (do ((var init [step]) ...) (test expr ...) command ...) => (values specs test exprs commands)
  (define (parse-do form)
    (unless (and (>= (length form) 3)
                 (list? (cadr form))
                 (non-empty-list? (caddr form)))
      (error 'parse-do "invalid do" form))
    (let loop ((specs (cadr form))
               (seen '()))
      (unless (null? specs)
        (let ((s (car specs)))
          (unless (and (list? s)
                       (<= 2 (length s) 3)
                       (symbol? (car s)))
            (error 'parse-do "invalid do binding" s))
          (when (memq (car s) seen)
            (error 'parse-do "duplicate do variable" (car s)))
          (loop (cdr specs) (cons (car s) seen)))))
    (values (cadr form) (caaddr form) (cdaddr form) (cdddr form)))


  ;; (begin expr ...) => (expr ...)
  (define (parse-begin form)
    (when (null? (cdr form))
      (error 'parse-begin "empty begin" form))
    (cdr form))


  ;; (set! var expr) => (values var expr)
  (define (parse-set! form)
    (unless (and (= (length form) 3)
                 (symbol? (cadr form)))
      (error 'parse-set! "invalid set!" form))
    (values (cadr form) (caddr form)))


  (define (define-form? form)
    (and (non-empty-list? form)
         (eq? (car form) 'define)))


  ;; (define (name arg ...) body ...) => (values name args body)
  ;; (define name init) => (values name #f (init))
  (define (parse-define form)
    (unless (define-form? form)
      (error 'parse-define "non define form in define position" form))
    (unless (and (>= (length form) 3)
                 (or (pair? (cadr form))
                     (and (symbol? (cadr form))
                          (= (length form) 3))))
      (error 'parse-define "invalid define" form))
    (if (symbol? (cadr form))
        (values (cadr form) #f (cddr form))
        (let ((name (caadr form))
              (args (cdadr form)))
          (unless (symbol? name)
            (error 'parse-define "invalid define name" form))
          (validate-parameters args)
          (values name args (cddr form)))))


  (define-record-type global-def
    (fields name index args body))


  ;; Map (make name i args body) over the define forms
  (define (scan-defs forms make)
    (let loop ((acc '())
               (names '())
               (forms forms)
               (i 0))
      (if (null? forms)
          (reverse acc)
          (let-values (((name args body) (parse-define (car forms))))
            (when (memq name names)
              (error 'scan-defs "redefinition not allowed" name))
            (loop (cons (make name i args body) acc)
                  (cons name names)
                  (cdr forms)
                  (+ i 1))))))


  ;; A define of the library: kinds are val or raw, result val, raw, void or never,
  ;; attrs low's def attributes; a procedure (only vals, attrs at most :public) is lifted
  ;; like a lambda, any other is a low def
  (define-record-type library-def
    (fields name args kinds result attrs body procedure? unsafe?))


  ;; (define-data name nwords)
  (define-record-type library-data
    (fields name nwords))


  ;; x or (raw x) => (x . kind)
  (define (parse-typed-param p)
    (cond
     ((symbol? p) (cons p 'val))
     ((and (list? p) (= (length p) 2) (eq? (car p) 'raw) (symbol? (cadr p)))
      (cons (cadr p) 'raw))
     (else (error 'parse-typed-param "invalid parameter" p))))


  ;; (declare decl ...) => (values result attrs unsafe?)
  (define (parse-declare decls)
    (let loop ((decls decls)
               (result 'val)
               (attrs '())
               (unsafe? #f))
      (if (null? decls)
          (values result (reverse attrs) unsafe?)
          (let ((d (car decls)))
            (cond
             ((memq d '(inline public native))
              (loop (cdr decls) result
                    (cons (string->symbol (format ":~a" d)) attrs) unsafe?))
             ((eq? d 'unsafe)
              (loop (cdr decls) result attrs #t))
             ((and (list? d) (= (length d) 2) (eq? (car d) 'result)
                   (memq (cadr d) '(val raw void never)))
              (loop (cdr decls) (cadr d) attrs unsafe?))
             (else (error 'parse-declare "invalid declaration" d)))))))


  ;; A define whose body starts with (declare ...)
  (define (declared-define? form)
    (and (define-form? form)
         (>= (length form) 3)
         (non-empty-list? (caddr form))
         (eq? (car (caddr form)) 'declare)))


  ;; x, (raw x), or the rest parameter (x . rest)
  (define (parse-typed-params ps)
    (cond
     ((null? ps) '())
     ((pair? ps) (cons (parse-typed-param (car ps)) (parse-typed-params (cdr ps))))
     ((symbol? ps) (list (cons ps 'rest)))
     (else (error 'parse-typed-param "invalid parameter" ps))))


  (define (parse-library-define form)
    (unless (and (define-form? form)
                 (>= (length form) 3)
                 (pair? (cadr form))
                 (symbol? (caadr form)))
      (error 'parse-library-define "invalid define" form))
    (let* ((params (parse-typed-params (cdadr form)))
           (body (cddr form))
           (declare? (and (non-empty-list? (car body)) (eq? (caar body) 'declare))))
      (validate-parameters (map car params))
      (when (and declare? (null? (cdr body)))
        (error 'parse-library-define "invalid define" form))
      (let-values (((result attrs unsafe?) (parse-declare (if declare? (cdar body) '()))))
        (let* ((vals? (and (for-all (lambda (p) (memq (cdr p) '(val rest))) params)
                           (eq? result 'val)))
               (procedure? (and vals? (for-all (lambda (a) (eq? a ':public)) attrs))))
          (when (and (memq ':public attrs) (not vals?))
            (error 'parse-library-define "public define with raw values" form))
          (when (and (or (not vals?) (memq ':native attrs)) (not unsafe?))
            (error 'parse-library-define "raw values or native without unsafe" form))
          (when (and (exists (lambda (p) (eq? (cdr p) 'rest)) params) (not procedure?))
            (error 'parse-library-define "rest parameter in a non-procedure define" form))
          (make-library-def (caadr form) (map car params) (map cdr params) result attrs
                            (if declare? (cdr body) body) procedure? unsafe?)))))


  (define (parse-define-data form)
    (unless (and (= (length form) 3)
                 (symbol? (cadr form))
                 (integer? (caddr form))
                 (exact? (caddr form))
                 (> (caddr form) 0))
      (error 'parse-define-data "invalid define-data" form))
    (make-library-data (cadr form) (caddr form)))


  ;; The library-def and library-data records of the library forms
  (define (scan-library forms)
    (let loop ((forms forms)
               (acc '())
               (names '()))
      (if (null? forms)
          (reverse acc)
          (let* ((form (car forms))
                 (x (if (and (non-empty-list? form) (eq? (car form) 'define-data))
                        (parse-define-data form)
                        (parse-library-define form)))
                 (name (if (library-def? x) (library-def-name x) (library-data-name x))))
            (when (memq name names)
              (error 'scan-library "redefinition not allowed" name))
            (loop (cdr forms) (cons x acc) (cons name names)))))))
