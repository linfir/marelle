(library (frontend)
  (export proc-counter cpl-program)
  (import (chezscheme)
          (utils)
          (ir))


  (define proc-counter
    (let ((n 0))
      (lambda ()
        (set! n (+ n 1))
        n)))


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


  (define *special-forms* '(if let))


  (define (special? env expr)
    (and (list? expr)
         (not (null? expr))
         (symbol? (car expr))
         (memq (car expr) *special-forms*)
         (not (assq (car expr) env))))


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

     ((number? expr)
      (error 'cpl-expr "unsupported number" expr))

     ((null? expr)
      (error 'cpl-expr "invalid form" expr))

     ((special? env expr)
      (case (car expr)
        ((if)
         (unless (= (length expr) 4)
           (error 'cpl-expr "invalid arity for if"))
         (cpl-if ctx env (list-ref expr 1) (list-ref expr 2) (list-ref expr 3)))

        ((let)
         (unless (= (length expr) 3)
           (error 'cpl-expr "invalid arity for let"))
         (cpl-expr ctx (extend-env-let ctx env (list-ref expr 1)) (list-ref expr 2)))

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


  ;; Compile expr in tail position: emit a ret or a tailcall.
  (define (cpl-tail ctx env expr)
    (cond
     ((special? env expr)
      (case (car expr)
        ((if)
         (unless (= (length expr) 4)
           (error 'cpl-tail "invalid arity for if"))
         (cpl-tail-if ctx env (list-ref expr 1) (list-ref expr 2) (list-ref expr 3)))

        ((let)
         (unless (= (length expr) 3)
           (error 'cpl-tail "invalid arity for let"))
         (cpl-tail ctx (extend-env-let ctx env (list-ref expr 1)) (list-ref expr 2)))

        (else
         (error 'cpl-tail "unimplemented special form" expr))))

     ((and (pair? expr) (list? expr))
      (cpl-tail-call ctx env expr))

     (else
      (emit! ctx (list 'ret (cpl-expr ctx env expr))))))


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


  (define (cpl-tail-if ctx env test-expr true-expr false-expr)
    (let* ((test-var (cpl-expr ctx env test-expr))
           (label-false (fresh-label ctx)))
      (emit! ctx
             (list 'cmp test-var '(imm #f))
             (list 'je label-false))
      (cpl-tail ctx env true-expr)
      (emit! ctx (list 'def label-false))
      (cpl-tail ctx env false-expr)))


  (define (extend-env-let ctx env bindings)
    (let loop ((bindings bindings)
               (new-env env))
      (if (null? bindings)
          new-env
          (let ((k (caar bindings))
                (v (cadar bindings)))
            (let ((dst (cpl-expr ctx env v)))
              (loop (cdr bindings)
                    (acons k dst new-env)))))))


  ;; Returns the code pointer and the argument variables.
  (define (cpl-call-operands ctx env form)
    (let ((vars (cpl-exprs ctx env form))
          (code (fresh-var ctx)))
      (emit! ctx
             (list 'ccall code "check_procedure" (car vars) (list 'quad (- (length vars) 1))))
      (values code (cdr vars))))


  (define (cpl-call ctx env form)
    (let-values (((code args) (cpl-call-operands ctx env form)))
      (let ((dst (fresh-var ctx)))
        (emit! ctx (cons* 'call dst code args))
        dst)))


  (define (cpl-tail-call ctx env form)
    (let-values (((code args) (cpl-call-operands ctx env form)))
      (emit! ctx (cons* 'tailcall code args))))


  (define (cpl env expr)
    (let ((ctx (new-expr-context)))
      (cpl-tail ctx env expr)
      (reverse (expr-context-code ctx))))


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
