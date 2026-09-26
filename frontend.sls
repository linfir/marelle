(library (frontend)
  (export proc-counter proc-name fn-name proc-data
          encode-immediate
          cpl-program)
  (import (chezscheme)
          (utils))


  (define proc-counter
    (let ((n 0))
      (lambda ()
        (set! n (+ n 1))
        n)))


  (define (proc-name n)
    (string-append "proc" (number->string n)))


  (define (fn-name n)
    (string-append "fn" (number->string n)))


  ;; procedure object: [type, arity, code]
  (define (proc-data proc arity code)
    (list 'data proc
          (list 'quad "procedure_type")
          (list 'quad arity)
          (list 'quad code)))


  (define-record-type expr-context
    (fields (mutable reg-counter)
            (mutable label-counter)
            (mutable code)))


  (define (new-expr-context)
    (make-expr-context 1 1 '()))


  (define (fresh-var ctx)
    (let ((n (expr-context-reg-counter ctx)))
      (expr-context-reg-counter-set! ctx (+ n 1))
      (list 'tmp n)))


  (define (fresh-label ctx)
    (let ((n (expr-context-label-counter ctx)))
      (expr-context-label-counter-set! ctx (+ n 1))
      (list 'label n)))


  (define (emit! ctx . instructions)
    (expr-context-code-set! ctx
                            (append (reverse instructions)
                                    (expr-context-code ctx))))


  (define int-bits 61)
  (define int-min (- (expt 2 (- int-bits 1))))
  (define int-max (- (expt 2 (- int-bits 1)) 1))


  (define (immediate? x)
    (or (and (integer? x)
             (<= int-min x int-max))
        (boolean? x)))


  (define (encode-immediate x)
    (define (tag t x)
      (+ t (* x 8)))
    (cond
     ((integer? x)
      (tag 1 x))
     ((boolean? x)
      (tag 2 (if x 1 0)))
     (else (error 'encode-immediate "non immediate value" x))))


  (define *special-forms* '(if let))


  ;; cpl expr
  ;;   env is an alist of pairs (symbol . variable)
  ;;   code is accumulated in ctx
  ;; returns dst or throw an error
  (define (cpl-expr ctx env expr)
    (cond
     ((immediate? expr)
      (let ((tmp (fresh-var ctx)))
        (emit! ctx (list 'mov tmp (list 'imm expr)))
        tmp))

     ((null? expr)
      (error 'cpl-expr "invalid form" expr))

     ((and (list? expr)
           (not (null? expr))
           (symbol? (car expr))
           (memq (car expr) *special-forms*)
           (not (assq (car expr) env)))
      (case (car expr)
        ((if)
         (unless (= (length expr) 4)
           (error 'cpl-expr "invalid arity for if"))
         (cpl-if ctx env (list-ref expr 1) (list-ref expr 2) (list-ref expr 3)))

        ((let)
         (unless (= (length expr) 3)
           (error 'cpl-expr "invalid arity for let"))
         (cpl-let ctx env (list-ref expr 1) (list-ref expr 2)))

        (else
         (error 'cpl-expr "unimplemented special form" expr))))

     ((and (list? expr)
           (not (null? expr)))
      (cpl-call ctx env expr))

     ((symbol? expr)
      (let ((x (assq expr env)))
        (if x
            (cdr x)
            (error 'cpl-expr "unknown symbol" expr))))

     (else
      (error 'cpl-expr "unknown form" expr))))


  (define (cpl-exprs ctx env exprs)
    (let loop ((exprs exprs)
               (vars '()))
      (if (null? exprs)
          (reverse vars)
          (let ((x (cpl-expr ctx env (car exprs))))
            (loop (cdr exprs) (cons x vars))))))


  (define (cpl-if ctx env test-expr true-expr false-expr)
    (let* ((test-var (cpl-expr ctx env test-expr))
           (label-false (fresh-label ctx)))
      (emit! ctx
             (list 'cmp test-var '(imm #f))
             (list 'je label-false))
      (let* ((true-var (cpl-expr ctx env true-expr))
             (dst-var (fresh-var ctx))
             (label-end (fresh-label ctx)))
        (emit! ctx
               (list 'mov dst-var true-var)
               (list 'jmp label-end)
               (list 'def label-false))
        (let ((false-var (cpl-expr ctx env false-expr)))
          (emit! ctx
                 (list 'mov dst-var false-var)
                 (list 'def label-end))
          dst-var))))


  (define (cpl-let ctx env bindings expr)
    (let loop ((bindings bindings)
               (new-env env))
      (if (null? bindings)
          (cpl-expr ctx new-env expr)
          (let ((k (caar bindings))
                (v (cadar bindings)))
            (let ((dst (cpl-expr ctx env v)))
              (loop (cdr bindings)
                    (acons k dst new-env)))))))


  (define (cpl-call ctx env form)
    (let* ((vars (cpl-exprs ctx env form))
           (a (fresh-var ctx))
           (b (fresh-var ctx)))
      (emit! ctx
             (list 'ccall a "check_procedure" (car vars) (list 'quad (- (length vars) 1)))
             (cons* 'call b a (cdr vars)))
      b))


  (define (cpl env expr)
    (let* ((ctx (new-expr-context))
           (x (cpl-expr ctx env expr)))
      (reverse (cons (list 'ret x) (expr-context-code ctx)))))


  ;; Convert a list of elements s.t.
  ;;   (define (foo x y) (+ x y)) ---> (foo 42 (x y) (+ x y))
  (define (scan-defs defs)
    (let loop ((acc '())
               (defs defs))
      (if (null? defs)
          (reverse acc)
          (let ((form (car defs)))
            (unless (and (list? form)
                         (not (null? form))
                         (eq? (car form) 'define))
              (error 'scan-defs "non define form in define position"))
            (let ((name (caadr form))
                  (args (cdadr form))
                  (body (caddr form)))
              (when (assq name acc)
                (error 'scan-defs "define: redefinition not allowed"))
              (loop (cons (list name (proc-counter) args body) acc)
                    (cdr defs)))))))


  (define (extend-env-with-args env args)
    (let loop ((i 1)
               (args args)
               (env env))
      (if (null? args)
          env
          (loop (+ i 1) (cdr args)
                (cons (list (car args) 'arg i) env)))))


  (define (cpl-function env proc args body)
    (cons* 'function proc
           (list 'arity (length args))
           (cpl (extend-env-with-args env args) body)))


  ;; TODO: what about a (define define 42) ?
  (define (cpl-program env program)
    (when (null? program)
      (error 'cpl-program "empty program"))

    (let-values (((defs expr) (split-last program)))
      (let* ((defs (scan-defs defs))
             (env (append (map (lambda (x) (cons (car x) (proc-name (cadr x)))) defs)
                          env))
             (data (map (lambda (x)
                          (proc-data (proc-name (cadr x)) (length (caddr x)) (fn-name (cadr x))))
                        defs))
             (fns (map (lambda (x)
                         (cpl-function env (fn-name (cadr x)) (caddr x) (cadddr x)))
                       defs)))
        (append data fns (list (cpl-function env "scheme_main" '() expr)))))))
