(library (parse)
  (export undef unbound immediate? exact-rational? quoted-datum? make-literal literal?
          literal-datum special? special-keyword source-keyword marker
          define-form? validate-parameters
          formals-names formals->params rename-formals
          parse-lambda parse-let parse-let* parse-letrec named-let? parse-named-let
          parse-if parse-cond parse-case parse-do parse-parameterize parse-begin parse-set!
          parse-define parse-let-values parse-define-values define-values-form?
          parse-define-record-type define-record-type-form? core-ref expand-and fresh-local
          scan-library
          library-def? library-def-name library-def-args library-def-kinds
          library-def-result library-def-attrs library-def-body library-def-procedure?
          library-def-unsafe? declared-define? parse-library-define parse-define-data
          library-data? library-data-name library-data-nwords
          make-library-constant library-constant? library-constant-name library-constant-kind
          library-constant-value define-constant-form?)
  (import (chezscheme)
          (utils))


  (define int-bits 61)
  (define int-min (- (expt 2 (- int-bits 1))))
  (define int-max (- (expt 2 (- int-bits 1)) 1))


  (define undef (void))


  ;; The value of a letrec variable before its init, which no program can make
  (define-record-type unbound-value)
  (define unbound (make-unbound-value))


  ;; A number literal: an integer of any size or a ratio; inexact ones are not supported
  (define (exact-rational? x)
    (and (rational? x) (exact? x)))


  (define (immediate? x)
    (or (and (integer? x)
             (exact? x)
             (<= int-min x int-max))
        (boolean? x)
        (char? x)
        (equal? x ''())
        (eq? x undef)
        (eq? x unbound)))


  (define (quoted-datum? x)
    (and (list? x) (= (length x) 2) (eq? (car x) 'quote)))


  ;; A symbol, pair, vector or bytevector datum after the front pass, loaded from its literal cell; a leaf
  ;; for the passes after it
  (define-record-type literal
    (fields datum))


  (define *special-forms*
    '(if cond case and or let let* letrec letrec* lambda begin set! when unless do define
         let-values let*-values define-values define-record-type quasiquote case-lambda
         parameterize))


  ;; Heads of the forms the compiler builds, so a program rebinding if or let keeps them;
  ;; fix, check-defined, closure, core, const, low and the cells have no source form
  (define markers
    (map-in-order (lambda (k) (cons (gensym (symbol->string k)) k))
         (append '(eq? fix check-defined closure core const low %cell %cell-ref %cell-set!)
                 *special-forms*)))


  (define (marker keyword)
    (car (find (lambda (m) (eq? (cdr m) keyword)) markers)))


  ;; (core name), the library def name, even where a program rebinds it
  (define (core-ref name)
    `(,(marker 'core) ,name))


  ;; A local with the pretty name of x, which names its closure
  (define (fresh-local x)
    (gensym (symbol->string x)))


  ;; The value of the deciding operand
  (define (expand-and exprs)
    (cond
     ((null? exprs) #t)
     ((null? (cdr exprs)) (car exprs))
     (else `(,(marker 'if) ,(car exprs) ,(expand-and (cdr exprs)) #f))))


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
  ;; formals with its names replaced by fresh, in order
  (define (rename-formals formals fresh)
    (cond
     ((null? formals) '())
     ((pair? formals) (cons (car fresh) (rename-formals (cdr formals) (cdr fresh))))
     (else (car fresh))))


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
            (compile-error 'validate-parameters "invalid parameter" arg))
          (when (memq arg seen)
            (compile-error 'validate-parameters "duplicate parameter" arg))
          (loop (cdr args) (cons arg seen))))))


  ;; (lambda formals body ...) => (values formals body)
  (define (parse-lambda form)
    (unless (>= (length form) 3)
      (compile-error 'parse-lambda "invalid lambda" form))
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
      (compile-error who "invalid let" form))
    (let loop ((bs (cadr form))
               (seen '()))
      (unless (null? bs)
        (let ((b (car bs)))
          (unless (and (list? b)
                       (= (length b) 2)
                       (symbol? (car b)))
            (compile-error who "invalid let binding" b))
          (when (and (not duplicates-ok?) (memq (car b) seen))
            (compile-error who "duplicate let variable" (car b)))
          (loop (cdr bs) (cons (car b) seen)))))
    (values (cadr form) (cddr form)))


  ;; (let-values ((formals init) ...) body ...), or let*-values => (values bindings body)
  (define (parse-let-values form duplicates-ok?)
    (unless (and (>= (length form) 3)
                 (list? (cadr form)))
      (compile-error 'parse-let-values "invalid let-values" form))
    (let loop ((bs (cadr form))
               (seen '()))
      (unless (null? bs)
        (let ((b (car bs)))
          (unless (and (list? b) (= (length b) 2))
            (compile-error 'parse-let-values "invalid let-values binding" b))
          (validate-parameters (car b))
          (let ((names (formals-names (car b))))
            (unless duplicates-ok?
              (for-each (lambda (x)
                          (when (memq x seen)
                            (compile-error 'parse-let-values "duplicate let-values variable" x)))
                        names))
            (loop (cdr bs) (append names seen))))))
    (values (cadr form) (cddr form)))


  (define (define-values-form? form)
    (and (non-empty-list? form)
         (eq? (car form) 'define-values)))


  ;; (define-values formals expr) => (values formals expr)
  (define (parse-define-values form)
    (unless (= (length form) 3)
      (compile-error 'parse-define-values "invalid define-values" form))
    (validate-parameters (cadr form))
    (values (cadr form) (caddr form)))


  ;; The subset of Chez's form the compiler uses:
  ;; (define-record-type name [(fields field ...)]), a field f or (mutable f)
  ;; => (values name ((f . mutable?) ...))
  (define (parse-define-record-type form)
    (define (invalid)
      (compile-error 'parse-define-record-type "invalid define-record-type" form))
    (unless (and (list? form) (<= 2 (length form) 3) (symbol? (cadr form)))
      (invalid))
    (let ((fields (if (null? (cddr form))
                      '()
                      (let ((clause (caddr form)))
                        (unless (and (pair? clause) (list? clause) (eq? (car clause) 'fields))
                          (invalid))
                        (map (lambda (f)
                               (cond
                                ((symbol? f) (cons f #f))
                                ((and (list? f) (= (length f) 2) (eq? (car f) 'mutable)
                                      (symbol? (cadr f)))
                                 (cons (cadr f) #t))
                                (else (invalid))))
                             (cdr clause))))))
      (let loop ((names (map car fields)))
        (unless (null? names)
          (when (memq (car names) (cdr names))
            (compile-error 'parse-define-record-type "duplicate field" (car names)))
          (loop (cdr names))))
      (values (cadr form) fields)))


  (define (define-record-type-form? form)
    (and (non-empty-list? form)
         (eq? (car form) 'define-record-type)))


  ;; (if test then [else]) => (values test then else)
  (define (parse-if form)
    (unless (<= 3 (length form) 4)
      (compile-error 'parse-if "invalid if" form))
    (values (cadr form)
            (caddr form)
            (if (null? (cdddr form)) undef (cadddr form))))


  ;; (cond clause ...) => (clause ...); else and => are checked by the caller
  (define (parse-cond form)
    (for-each (lambda (c)
                (unless (non-empty-list? c)
                  (compile-error 'parse-cond "invalid cond clause" c)))
              (cdr form))
    (cdr form))


  ;; (case key clause ...) => (values key clauses); else and => are checked by the caller
  (define (parse-case form)
    (unless (>= (length form) 2)
      (compile-error 'parse-case "invalid case" form))
    (for-each (lambda (c)
                (unless (and (non-empty-list? c) (pair? (cdr c)))
                  (compile-error 'parse-case "invalid case clause" c)))
              (cddr form))
    (values (cadr form) (cddr form)))


  ;; (do ((var init [step]) ...) (test expr ...) command ...) => (values specs test exprs commands)
  (define (parse-do form)
    (unless (and (>= (length form) 3)
                 (list? (cadr form))
                 (non-empty-list? (caddr form)))
      (compile-error 'parse-do "invalid do" form))
    (let loop ((specs (cadr form))
               (seen '()))
      (unless (null? specs)
        (let ((s (car specs)))
          (unless (and (list? s)
                       (<= 2 (length s) 3)
                       (symbol? (car s)))
            (compile-error 'parse-do "invalid do binding" s))
          (when (memq (car s) seen)
            (compile-error 'parse-do "duplicate do variable" (car s)))
          (loop (cdr specs) (cons (car s) seen)))))
    (values (cadr form) (caaddr form) (cdaddr form) (cdddr form)))


  ;; (parameterize ((param value) ...) body ...) => (values bindings body)
  (define (parse-parameterize form)
    (unless (and (>= (length form) 3)
                 (list? (cadr form))
                 (for-all (lambda (b) (and (list? b) (= (length b) 2))) (cadr form)))
      (compile-error 'parse-parameterize "invalid parameterize" form))
    (values (cadr form) (cddr form)))


  ;; (begin expr ...) => (expr ...)
  (define (parse-begin form)
    (when (null? (cdr form))
      (compile-error 'parse-begin "empty begin" form))
    (cdr form))


  ;; (set! var expr) => (values var expr)
  (define (parse-set! form)
    (unless (and (= (length form) 3)
                 (symbol? (cadr form)))
      (compile-error 'parse-set! "invalid set!" form))
    (values (cadr form) (caddr form)))


  (define (define-form? form)
    (and (non-empty-list? form)
         (eq? (car form) 'define)))


  ;; (define (name arg ...) body ...) => (values name args body)
  ;; (define name init) => (values name #f (init))
  (define (parse-define form)
    (unless (define-form? form)
      (compile-error 'parse-define "non define form in define position" form))
    (unless (and (>= (length form) 3)
                 (or (pair? (cadr form))
                     (and (symbol? (cadr form))
                          (= (length form) 3))))
      (compile-error 'parse-define "invalid define" form))
    (if (symbol? (cadr form))
        (values (cadr form) #f (cddr form))
        (let ((name (caadr form))
              (args (cdadr form)))
          (unless (symbol? name)
            (compile-error 'parse-define "invalid define name" form))
          (validate-parameters args)
          (values name args (cddr form)))))


  ;; A define of the library: kinds are val or raw, result val, raw, void or never,
  ;; attrs low's def attributes; a procedure (only vals, attrs at most :public) is lifted
  ;; like a lambda, any other is a low def
  (define-record-type library-def
    (fields name args kinds result attrs body procedure? unsafe?))


  ;; (define-data name nwords)
  (define-record-type library-data
    (fields name nwords))


  ;; (define-constant name e) or (define-constant name (raw e)), kind val or raw, e folded
  (define-record-type library-constant
    (fields name kind value))


  ;; x or (raw x) => (x . kind)
  (define (parse-typed-param p)
    (cond
     ((symbol? p) (cons p 'val))
     ((and (list? p) (= (length p) 2) (eq? (car p) 'raw) (symbol? (cadr p)))
      (cons (cadr p) 'raw))
     (else (compile-error 'parse-typed-param "invalid parameter" p))))


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
             ((memq d '(inline public noalloc))
              (loop (cdr decls) result
                    (cons (string->symbol (format ":~a" d)) attrs) unsafe?))
             ((eq? d 'unsafe)
              (loop (cdr decls) result attrs #t))
             ((and (list? d) (= (length d) 2) (eq? (car d) 'result)
                   (memq (cadr d) '(val raw void never)))
              (loop (cdr decls) (cadr d) attrs unsafe?))
             (else (compile-error 'parse-declare "invalid declaration" d)))))))


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
     (else (compile-error 'parse-typed-param "invalid parameter" ps))))


  (define (parse-library-define form)
    (unless (and (define-form? form)
                 (>= (length form) 3)
                 (pair? (cadr form))
                 (symbol? (caadr form)))
      (compile-error 'parse-library-define "invalid define" form))
    (let* ((params (parse-typed-params (cdadr form)))
           (body (cddr form))
           (declare? (and (non-empty-list? (car body)) (eq? (caar body) 'declare))))
      (validate-parameters (map car params))
      (when (and declare? (null? (cdr body)))
        (compile-error 'parse-library-define "invalid define" form))
      (let-values (((result attrs unsafe?) (parse-declare (if declare? (cdar body) '()))))
        (let* ((vals? (and (for-all (lambda (p) (memq (cdr p) '(val rest))) params)
                           (eq? result 'val)))
               (procedure? (and vals? (for-all (lambda (a) (eq? a ':public)) attrs))))
          (when (and (memq ':public attrs) (not vals?))
            (compile-error 'parse-library-define "public define with raw values" form))
          (when (and (or (eq? result 'raw) (memq 'raw (map cdr params)) (memq ':noalloc attrs))
                     (not unsafe?))
            (compile-error 'parse-library-define "raw values or noalloc without unsafe" form))
          (when (and (exists (lambda (p) (eq? (cdr p) 'rest)) params) (not procedure?))
            (compile-error 'parse-library-define "rest parameter in a non-procedure define" form))
          (make-library-def (caadr form) (map car params) (map cdr params) result attrs
                            (if declare? (cdr body) body) procedure? unsafe?)))))


  (define (parse-define-data form)
    (unless (and (= (length form) 3)
                 (symbol? (cadr form))
                 (integer? (caddr form))
                 (exact? (caddr form))
                 (> (caddr form) 0))
      (compile-error 'parse-define-data "invalid define-data" form))
    (make-library-data (cadr form) (caddr form)))


  (define (define-constant-form? form)
    (and (non-empty-list? form) (eq? (car form) 'define-constant)))


  (define constant-ops
    `((+ . ,+) (- . ,-) (* . ,*) (ash . ,ash) (quotient . ,quotient)
      (bitwise-and . ,bitwise-and) (bitwise-ior . ,bitwise-ior) (bitwise-xor . ,bitwise-xor)))


  ;; The value of e: literals, the constants (name . value), and constant-ops
  (define (fold-constant e constants form)
    (cond
     ((or (integer? e) (boolean? e) (char? e)) e)
     ((and (symbol? e) (assq e constants)) => cdr)
     ((and (non-empty-list? e) (assq (car e) constant-ops))
      => (lambda (op)
           (let ((args (map (lambda (x) (fold-constant x constants form)) (cdr e))))
             (unless (for-all integer? args)
               (compile-error 'parse-define-constant "non integer operand" form))
             (apply (cdr op) args))))
     (else (compile-error 'parse-define-constant "invalid constant expression" form))))


  (define (parse-define-constant form constants)
    (unless (and (= (length form) 3) (symbol? (cadr form)))
      (compile-error 'parse-define-constant "invalid define-constant" form))
    (let* ((e (caddr form))
           (raw? (and (non-empty-list? e) (eq? (car e) 'raw) (= (length e) 2)))
           (value (fold-constant (if raw? (cadr e) e) constants form)))
      (if raw?
          (unless (and (integer? value) (<= (- (expt 2 63)) value (- (expt 2 64) 1)))
            (compile-error 'parse-define-constant "invalid raw constant" form))
          (unless (immediate? value)
            (compile-error 'parse-define-constant "invalid constant" form)))
      (make-library-constant (cadr form) (if raw? 'raw 'val) value)))


  ;; The library-def, library-data and library-constant records of the library forms
  (define (scan-library forms)
    (let loop ((forms forms)
               (acc '())
               (names '()))
      (if (null? forms)
          (reverse acc)
          (let* ((form (car forms))
                 (x (cond
                     ((and (non-empty-list? form) (eq? (car form) 'define-data))
                      (parse-define-data form))
                     ((define-constant-form? form)
                      (parse-define-constant
                       form
                       (map (lambda (c) (cons (library-constant-name c) (library-constant-value c)))
                            (filter library-constant? acc))))
                     (else (parse-library-define form))))
                 (name (cond
                        ((library-def? x) (library-def-name x))
                        ((library-data? x) (library-data-name x))
                        (else (library-constant-name x)))))
            (when (memq name names)
              (compile-error 'scan-library "redefinition not allowed" name))
            (loop (cdr forms) (cons x acc) (cons name names))))))


  ;; Printed quoted, in dumps
  (add-printer! literal? (lambda (r) (list 'quote (literal-datum r)))))
