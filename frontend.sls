(library (frontend)
  (export program->ir)
  (import (chezscheme)
          (utils)
          (ir))


  (define-record-type program-context
    (fields (mutable entries)))


  (define (new-program-context)
    (make-program-context '()))


  (define (add-entry! pctx entry)
    (program-context-entries-set! pctx (cons entry (program-context-entries pctx))))


  (define-record-type expr-context
    (fields program
            reg-counter
            label-counter
            (mutable code)))


  (define (new-expr-context pctx)
    (make-expr-context pctx (make-counter) (make-counter) '()))


  (define (fresh-var ctx)
    (list 'tmp ((expr-context-reg-counter ctx))))


  (define (fresh-label ctx)
    (list 'label ((expr-context-label-counter ctx))))


  (define (emit! ctx instruction)
    (expr-context-code-set! ctx
                            (cons instruction (expr-context-code ctx))))


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


  ;; (lambda (arg ...) body) => (values args body)
  (define (parse-lambda form)
    (unless (= (length form) 3)
      (error 'parse-lambda "invalid lambda" form))
    (validate-parameters (cadr form))
    (values (cadr form) (caddr form)))


  ;; (let ((var init) ...) body) => (values bindings body)
  (define (parse-let form)
    (unless (and (= (length form) 3)
                 (list? (cadr form)))
      (error 'parse-let "invalid let" form))
    (let loop ((bs (cadr form))
               (seen '()))
      (unless (null? bs)
        (let ((b (car bs)))
          (unless (and (list? b)
                       (= (length b) 2)
                       (symbol? (car b)))
            (error 'parse-let "invalid let binding" b))
          (when (memq (car b) seen)
            (error 'parse-let "duplicate let variable" (car b)))
          (loop (cdr bs) (cons (car b) seen)))))
    (values (cadr form) (caddr form)))


  ;; compile expr in value position
  ;;   env is an alist of pairs (symbol . variable)
  ;;   code is accumulated in ctx
  ;; returns dst or throw an error
  (define (compile-value ctx env expr)
    (cond
     ((immediate? expr)
      (let ((tmp (fresh-var ctx)))
        (emit! ctx `(mov ,tmp (imm ,expr)))
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
         (compile-value-if ctx env (cadr expr) (caddr expr) (cadddr expr)))

        ((let)
         (let-values (((bindings body) (parse-let expr)))
           (compile-value ctx (bind-let-vars ctx env bindings) body)))

        ((lambda)
         (compile-lambda ctx env expr))

        (else
         (error 'compile-value "unimplemented special form" expr))))

     ((and (list? expr) (not (null? expr)))
      (compile-value-call ctx env expr))

     ((symbol? expr)
      (let ((x (assq expr env)))
        (if x
            (cdr x)
            (error 'compile-value "unknown symbol" expr))))

     (else
      (error 'compile-value "unknown form" expr))))


  (define (compile-tail ctx env expr)
    (cond
     ((special? env expr)
      (case (car expr)
        ((if)
         (unless (= (length expr) 4)
           (error 'compile-tail "invalid arity for if"))
         (compile-tail-if ctx env (cadr expr) (caddr expr) (cadddr expr)))

        ((let)
         (let-values (((bindings body) (parse-let expr)))
           (compile-tail ctx (bind-let-vars ctx env bindings) body)))

        ((lambda)
         (emit! ctx `(ret ,(compile-value ctx env expr))))

        (else
         (error 'compile-tail "unimplemented special form" expr))))

     ((and (list? expr) (not (null? expr)) (not (immediate? expr)))
      (compile-tail-call ctx env expr))

     (else
      (emit! ctx `(ret ,(compile-value ctx env expr))))))


  (define (captured-vars env expr)
    (define (bind env names)
      (append (map (lambda (x) (cons x 'bound)) names) env))

    (let walk ((env env)
               (expr expr)
               (acc '()))
      (cond
       ((symbol? expr)
        (let ((x (assq expr env)))
          (if (and x
                   (not (eq? (cdr x) 'bound))
                   (not (string? (cdr x)))
                   (not (memq expr acc)))
              (cons expr acc)
              acc)))

       ((special? env expr)
        (case (car expr)
          ((let)
           (let-values (((bindings body) (parse-let expr)))
             (let ((acc (fold-left (lambda (acc b) (walk env (cadr b) acc))
                                   acc bindings)))
               (walk (bind env (map car bindings)) body acc))))

          ((lambda)
           (let-values (((args body) (parse-lambda expr)))
             (walk (bind env args) body acc)))

          (else
           (fold-left (lambda (acc x) (walk env x acc)) acc (cdr expr)))))

       ((list? expr)
        (fold-left (lambda (acc x) (walk env x acc)) acc expr))

       (else acc))))


  (define (compile-lambda ctx env expr)
    (let-values (((args body) (parse-lambda expr)))
      (let ((captures (reverse (captured-vars env expr))))
        (unless (null? captures)
          (error 'compile-lambda "unsupported captured variables" captures)))
      (let ((n (proc-counter))
            (pctx (expr-context-program ctx))
            (body-env (filter (lambda (b) (string? (cdr b))) env)))
        (add-entry! pctx (proc-data (proc-name n) (length args) (fn-name n)))
        (add-entry! pctx (function->ir pctx body-env (fn-name n) args body))
        (proc-name n))))


  (define (compile-values ctx env exprs)
    (map-in-order (lambda (expr)
                    (compile-value ctx env expr))
                  exprs))


  (define (compile-value-if ctx env test-expr true-expr false-expr)
    (let* ((test-var (compile-value ctx env test-expr))
           (dst-var (fresh-var ctx))
           (label-false (fresh-label ctx))
           (label-end (fresh-label ctx)))
      (emit! ctx `(cmp ,test-var (imm #f)))
      (emit! ctx `(je ,label-false))
      (emit! ctx `(mov ,dst-var ,(compile-value ctx env true-expr)))
      (emit! ctx `(jmp ,label-end))
      (emit! ctx `(def ,label-false))
      (emit! ctx `(mov ,dst-var ,(compile-value ctx env false-expr)))
      (emit! ctx `(def ,label-end))
      dst-var))


  (define (compile-tail-if ctx env test-expr true-expr false-expr)
    (let* ((test-var (compile-value ctx env test-expr))
           (label-false (fresh-label ctx)))
      (emit! ctx `(cmp ,test-var (imm #f)))
      (emit! ctx `(je ,label-false))
      (compile-tail ctx env true-expr)
      (emit! ctx `(def ,label-false))
      (compile-tail ctx env false-expr)))


  (define (bind-let-vars ctx env bindings)
    (let loop ((bindings bindings)
               (new-env env))
      (if (null? bindings)
          new-env
          (let ((k (caar bindings))
                (v (cadar bindings)))
            (let ((dst (compile-value ctx env v)))
              (loop (cdr bindings)
                    (acons k dst new-env)))))))


  (define (compile-operands ctx env form)
    (let* ((vars (compile-values ctx env form))
           (code (fresh-var ctx)))
      (emit! ctx `(ccall ,code "check_procedure" ,(car vars) (quad ,(- (length vars) 1))))
      (cons code vars)))


  (define (compile-value-call ctx env form)
    (let* ((operands (compile-operands ctx env form))
           (dst (fresh-var ctx)))
      (emit! ctx `(call ,dst ,@operands))
      dst))


  (define (compile-tail-call ctx env form)
    (emit! ctx `(tailcall ,@(compile-operands ctx env form))))


  (define-record-type def
    (fields name number params body))


  (define (find-def name defs)
    (find (lambda (d) (eq? (def-name d) name)) defs))


  ;; Convert each (define (foo x y) (+ x y)) into a def record
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
            (unless (and (= (length form) 3)
                         (pair? (cadr form)))
              (error 'scan-defs "define: invalid form" form))
            (let ((name (caadr form))
                  (args (cdadr form))
                  (body (caddr form)))
              (unless (symbol? name)
                (error 'scan-defs "define: invalid name" form))
              (validate-parameters args)
              (when (find-def name acc)
                (error 'scan-defs "define: redefinition not allowed"))
              (loop (cons (make-def name (proc-counter) args body) acc)
                    (cdr defs)))))))


  (define (extend-env-with-args env args)
    (let loop ((i 1)
               (args args)
               (env env))
      (if (null? args)
          env
          (loop (+ i 1) (cdr args)
                (cons (list (car args) 'arg i) env)))))


  (define (function->ir pctx env proc args body)
    (let ((ctx (new-expr-context pctx)))
      (compile-tail ctx (extend-env-with-args env args) body)
      `(function ,proc
                 (arity ,(length args))
                 ,@(reverse (expr-context-code ctx)))))


  (define (program->ir env program)
    (when (null? program)
      (error 'program->ir "empty program"))

    (let-values (((defs expr) (split-last program)))
      (let* ((defs (scan-defs defs))
             (env (append (map (lambda (d) (cons (def-name d) (proc-name (def-number d)))) defs)
                          env))
             (pctx (new-program-context)))
        (for-each (lambda (d)
                    (let ((n (def-number d))
                          (args (def-params d)))
                      (add-entry! pctx (proc-data (proc-name n) (length args) (fn-name n)))
                      (add-entry! pctx (function->ir pctx env (fn-name n) args (def-body d)))))
                  defs)
        (add-entry! pctx (function->ir pctx env "scheme_main" '() expr))
        (reverse (program-context-entries pctx))))))
