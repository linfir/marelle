(library (frontend)
  (export program->ir)
  (import (chezscheme)
          (utils)
          (ir))


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


  (define *special-forms* '(if let lambda))


  (define (special? env expr)
    (and (list? expr)
         (not (null? expr))
         (symbol? (car expr))
         (memq (car expr) *special-forms*)
         (not (assq (car expr) env))))


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


  (define (validate-lambda expr)
    (unless (= (length expr) 3)
      (error 'validate-lambda "invalid lambda" expr))
    (validate-parameters (cadr expr)))


  ;; compile expr in value position
  ;;   env is an alist of pairs (symbol . variable)
  ;;   code is accumulated in ctx
  ;; returns dst or throw an error
  (define (compile-value ctx env expr)
    (cond
     ((immediate? expr)
      (let ((tmp (fresh-var ctx)))
        (emit! ctx (list 'mov tmp (list 'imm expr)))
        tmp))

     ((number? expr)
      (error 'compile-value "unsupported number" expr))

     ((null? expr)
      (error 'compile-value "invalid form" expr))

     ((special? env expr)
      (case (car expr)
        ((if)
         (unless (= (length expr) 4)
           (error 'compile-value "invalid arity for if"))
         (compile-value-if ctx env (list-ref expr 1) (list-ref expr 2) (list-ref expr 3)))

        ((let)
         (unless (= (length expr) 3)
           (error 'compile-value "invalid arity for let"))
         (compile-value ctx (compile-let-bindings ctx env (list-ref expr 1)) (list-ref expr 2)))

        ((lambda)
         (validate-lambda expr)
         (error 'compile-value "unimplemented special form" expr))

        (else
         (error 'compile-value "unimplemented special form" expr))))

     ((and (list? expr)
           (not (null? expr)))
      (compile-value-call ctx env expr))

     ((symbol? expr)
      (let ((x (assq expr env)))
        (if x
            (cdr x)
            (error 'compile-value "unknown symbol" expr))))

     (else
      (error 'compile-value "unknown form" expr))))


  ;; Compile expr in tail position: emit a ret or a tailcall.
  (define (compile-tail ctx env expr)
    (cond
     ((special? env expr)
      (case (car expr)
        ((if)
         (unless (= (length expr) 4)
           (error 'compile-tail "invalid arity for if"))
         (compile-tail-if ctx env (list-ref expr 1) (list-ref expr 2) (list-ref expr 3)))

        ((let)
         (unless (= (length expr) 3)
           (error 'compile-tail "invalid arity for let"))
         (compile-tail ctx (compile-let-bindings ctx env (list-ref expr 1)) (list-ref expr 2)))

        ((lambda)
         (emit! ctx (list 'ret (compile-value ctx env expr))))

        (else
         (error 'compile-tail "unimplemented special form" expr))))

     ((and (pair? expr) (list? expr) (not (immediate? expr)))
      (compile-tail-call ctx env expr))

     (else
      (emit! ctx (list 'ret (compile-value ctx env expr))))))


  (define (compile-values ctx env exprs)
    (let loop ((exprs exprs)
               (vars '()))
      (if (null? exprs)
          (reverse vars)
          (let ((x (compile-value ctx env (car exprs))))
            (loop (cdr exprs) (cons x vars))))))


  (define (compile-value-if ctx env test-expr true-expr false-expr)
    (let* ((test-var (compile-value ctx env test-expr))
           (label-false (fresh-label ctx)))
      (emit! ctx
             (list 'cmp test-var '(imm #f))
             (list 'je label-false))
      (let* ((true-var (compile-value ctx env true-expr))
             (dst-var (fresh-var ctx))
             (label-end (fresh-label ctx)))
        (emit! ctx
               (list 'mov dst-var true-var)
               (list 'jmp label-end)
               (list 'def label-false))
        (let ((false-var (compile-value ctx env false-expr)))
          (emit! ctx
                 (list 'mov dst-var false-var)
                 (list 'def label-end))
          dst-var))))


  (define (compile-tail-if ctx env test-expr true-expr false-expr)
    (let* ((test-var (compile-value ctx env test-expr))
           (label-false (fresh-label ctx)))
      (emit! ctx
             (list 'cmp test-var '(imm #f))
             (list 'je label-false))
      (compile-tail ctx env true-expr)
      (emit! ctx (list 'def label-false))
      (compile-tail ctx env false-expr)))


  (define (compile-let-bindings ctx env bindings)
    (let loop ((bindings bindings)
               (new-env env))
      (if (null? bindings)
          new-env
          (let ((k (caar bindings))
                (v (cadar bindings)))
            (let ((dst (compile-value ctx env v)))
              (loop (cdr bindings)
                    (acons k dst new-env)))))))


  ;; Returns the code pointer, the procedure and the argument variables.
  (define (compile-operands ctx env form)
    (let ((vars (compile-values ctx env form))
          (code (fresh-var ctx)))
      (emit! ctx
             (list 'ccall code "check_procedure" (car vars) (list 'quad (- (length vars) 1))))
      (values code (car vars) (cdr vars))))


  (define (compile-value-call ctx env form)
    (let-values (((code proc args) (compile-operands ctx env form)))
      (let ((dst (fresh-var ctx)))
        (emit! ctx (cons* 'call dst code proc args))
        dst)))


  (define (compile-tail-call ctx env form)
    (let-values (((code proc args) (compile-operands ctx env form)))
      (emit! ctx (cons* 'tailcall code proc args))))


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
              (validate-parameters args)
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


  (define (function->ir env proc args body)
    (let ((ctx (new-expr-context)))
      (compile-tail ctx (extend-env-with-args env args) body)
      (cons* 'function proc
             (list 'arity (length args))
             (reverse (expr-context-code ctx)))))


  ;; TODO: what about a (define define 42) ?
  (define (program->ir env program)
    (when (null? program)
      (error 'program->ir "empty program"))

    (let-values (((defs expr) (split-last program)))
      (let* ((defs (scan-defs defs))
             (env (append (map (lambda (x) (cons (car x) (proc-name (cadr x)))) defs)
                          env))
             (data (map (lambda (x)
                          (proc-data (proc-name (cadr x)) (length (caddr x)) (fn-name (cadr x))))
                        defs))
             (fns (map (lambda (x)
                         (function->ir env (fn-name (cadr x)) (caddr x) (cadddr x)))
                       defs)))
        (append data fns (list (function->ir env "scheme_main" '() expr)))))))
