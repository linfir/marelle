(library (compile-parse)
  (export undef immediate? special? call? define-form?
          parse-lambda parse-let parse-let* parse-if parse-begin parse-set!
          scan-defs scan-prelude
          make-global-def global-def-name global-def-index global-def-args global-def-body
          prelude-def-name prelude-def-args prelude-def-body)
  (import (chezscheme)
          (utils))


  (define int-bits 61)
  (define int-min (- (expt 2 (- int-bits 1))))
  (define int-max (- (expt 2 (- int-bits 1)) 1))


  (define undef (void))


  (define (immediate? x)
    (or (and (integer? x)
             (exact? x)
             (<= int-min x int-max))
        (boolean? x)
        (equal? x ''())
        (eq? x undef)))


  (define *special-forms* '(if and or let let* lambda begin set!))


  (define (special? env expr)
    (and (non-empty-list? expr)
         (let ((x (car expr)))
           (and (symbol? x)
                (memq x *special-forms*)
                (not (assq x env))))))


  (define (validate-parameters args)
    (unless (list? args)
      (error 'validate-parameters "invalid parameter list" args))
    (let loop ((args args)
               (seen '()))
      (unless (null? args)
        (let ((arg (car args)))
          (unless (symbol? arg)
            (error 'validate-parameters "invalid parameter" arg))
          (when (memq arg seen)
            (error 'validate-parameters "duplicate parameter" arg))
          (loop (cdr args) (cons arg seen)))))
    (when (> (length args) 6)
      (error 'validate-parameters "too many parameters" args)))


  ;; (lambda (arg ...) body ...) => (values args body)
  (define (parse-lambda form)
    (unless (>= (length form) 3)
      (error 'parse-lambda "invalid lambda" form))
    (validate-parameters (cadr form))
    (values (cadr form) (cddr form)))


  ;; (let ((var init) ...) body ...) => (values bindings body)
  (define (parse-let form)
    (parse-bindings 'parse-let form #f))


  ;; (let* ((var init) ...) body ...) => (values bindings body)
  (define (parse-let* form)
    (parse-bindings 'parse-let* form #t))


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


  ;; (if test then [else]) => (values test then else)
  (define (parse-if form)
    (unless (<= 3 (length form) 4)
      (error 'parse-if "invalid if" form))
    (values (cadr form)
            (caddr form)
            (if (null? (cdddr form)) undef (cadddr form))))


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


  ;; An application: neither an immediate nor a special form
  (define (call? env expr)
    (and (non-empty-list? expr)
         (not (immediate? expr))
         (not (special? env expr))))


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


  (define-record-type prelude-def
    (fields name args body))


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


  (define (scan-prelude forms)
    (scan-defs forms
               (lambda (name i args body)
                 (unless args
                   (error 'scan-prelude "value define in the prelude" name))
                 (make-prelude-def name args body)))))
