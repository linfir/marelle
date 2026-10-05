(define-record-type program-context
  (fields proc-counter (mutable entries)))


(define (new-program-context)
  (make-program-context (make-counter) '()))


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


(define (expr-context->function ctx name arity)
  `(function ,name
             (arity ,arity)
             ,@(reverse (expr-context-code ctx))))


;; Bindings of the lexical environment
(define-record-type local-binding
  (fields operand))


(define-record-type procedure-binding
  (fields proc arity))


;; Top-level define, cell index of "scheme_globals"
(define-record-type global-binding
  (fields index))


;; name gives the symbol names, and the C function called without an inliner
(define-record-type primitive-binding
  (parent procedure-binding)
  (fields name inliner))


;; A name bound inside the analysed expression, for captured-vars
(define-record-type bound-binding)


;; The operand for a local, the procedure object symbol for a procedure;
;; not for globals, which need a load
(define (binding-value binding)
  (if (local-binding? binding)
      (local-binding-operand binding)
      (procedure-binding-proc binding)))


(define *special-forms* '(if and or let let* lambda begin))


(define (special? env expr)
  (and (non-empty-list? expr)
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


;; (if test then [else]) => (values test then else), else defaults to undef
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


;; compile expr in value position
;;   env is an alist of pairs (symbol . binding)
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

   ((special? env expr)
    (case (car expr)
      ((if)
       (let-values (((test then else) (parse-if expr)))
         (compile-value-if ctx env test then else)))

      ((and)
       (compile-value-logic ctx env (cdr expr) #t 'je))

      ((or)
       (compile-value-logic ctx env (cdr expr) #f 'jne))

      ((let let* begin)
       (let-values (((env last) (compile-scope ctx env expr)))
         (compile-value ctx env last)))

      ((lambda)
       (compile-lambda ctx env expr))

      (else
       (error 'compile-value "unimplemented special form" expr))))

   ((call? env expr)
    (compile-value-call ctx env expr))

   ((symbol? expr)
    (let ((x (assq expr env)))
      (cond
       ((not x)
        (error 'compile-value "unknown symbol" expr))
       ((global-binding? (cdr x))
        (let ((tmp (fresh-var ctx)))
          (emit! ctx `(load ,tmp "scheme_globals" ,(global-binding-index (cdr x))))
          tmp))
       (else
        (binding-value (cdr x))))))

   (else
    (error 'compile-value "unknown form" expr))))


;; Only forms passing the tail position on are handled here
(define (compile-tail ctx env expr)
  (case (and (special? env expr) (car expr))
    ((if)
     (let-values (((test then else) (parse-if expr)))
       (compile-tail-if ctx env test then else)))

    ((and)
     (compile-tail-logic ctx env (cdr expr) #t 'je))

    ((or)
     (compile-tail-logic ctx env (cdr expr) #f 'jne))

    ((let let* begin)
     (let-values (((env last) (compile-scope ctx env expr)))
       (compile-tail ctx env last)))

    (else
     (if (call? env expr)
         (compile-tail-call ctx env expr)
         (emit! ctx `(ret ,(compile-value ctx env expr)))))))


(define (call? env expr)
  (and (non-empty-list? expr)
       (not (immediate? expr))
       (not (special? env expr))))


;; let, let* and begin: compile all but the last body expression,
;; which is left to the caller => (values env last)
(define (compile-scope ctx env expr)
  (case (car expr)
    ((let)
     (let-values (((bindings body) (parse-let expr)))
       (compile-body ctx (bind-let-vars ctx env bindings) body)))

    ((let*)
     (let-values (((bindings body) (parse-let* expr)))
       (compile-body ctx (bind-let*-vars ctx env bindings) body)))

    ((begin)
     (compile-body ctx env (parse-begin expr)))))


;; Compile each expression but the last, discarding their values
;; => (values env last)
(define (compile-body ctx env exprs)
  (let-values (((init last) (split-last exprs)))
    (for-each (lambda (expr) (compile-value ctx env expr)) init)
    (values env last)))


(define (captured-vars env args body)
  (define (bind env names)
    (append (map (lambda (x) (cons x (make-bound-binding))) names) env))

  (define (walk-list env exprs acc)
    (fold-left (lambda (acc x) (walk env x acc)) acc exprs))

  (define (walk env expr acc)
    (cond
     ((symbol? expr)
      (let ((x (assq expr env)))
        (if (and x
                 (local-binding? (cdr x))
                 (not (memq expr acc)))
            (cons expr acc)
            acc)))

     ((special? env expr)
      (case (car expr)
        ((let)
         (let-values (((bindings body) (parse-let expr)))
           (let ((acc (fold-left (lambda (acc b) (walk env (cadr b) acc))
                                 acc bindings)))
             (walk-list (bind env (map car bindings)) body acc))))

        ((let*)
         (let-values (((bindings body) (parse-let* expr)))
           (let loop ((env env)
                      (bindings bindings)
                      (acc acc))
             (if (null? bindings)
                 (walk-list env body acc)
                 (loop (bind env (list (caar bindings)))
                       (cdr bindings)
                       (walk env (cadar bindings) acc))))))

        ((lambda)
         (let-values (((args body) (parse-lambda expr)))
           (walk-list (bind env args) body acc)))

        (else
         (walk-list env (cdr expr) acc))))

     ((list? expr)
      (walk-list env expr acc))

     (else acc)))

  (walk-list (bind env args) body '()))


(define (compile-lambda ctx env expr)
  (let-values (((args body) (parse-lambda expr)))
    (compile-procedure ctx env args body)))


(define (compile-procedure ctx env args body)
  (let* ((captures (reverse (captured-vars env args body)))
         (pctx (expr-context-program ctx))
         (n ((program-context-proc-counter pctx)))
         (body-env (filter (lambda (b) (not (local-binding? (cdr b)))) env)))
    (add-entry! pctx (function->ir pctx body-env (fn-name n) captures args body))
    (if (null? captures)
        (begin
          (add-entry! pctx (proc-data (proc-name n) (length args) (fn-name n)))
          (proc-name n))
        (make-closure ctx env (fn-name n) (length args) captures))))


;; Allocate [type, arity, code, ncaptures, captures...]
(define (make-closure ctx env code arity captures)
  (let ((dst (fresh-var ctx))
        (ncaptures (length captures)))
    (emit! ctx `(alloc ,dst ,(+ 4 ncaptures) 0))
    (emit! ctx `(store ,dst 0 "procedure_type"))
    (emit! ctx `(store ,dst 1 (quad ,arity)))
    (emit! ctx `(store ,dst 2 ,code))
    (emit! ctx `(store ,dst 3 (quad ,ncaptures)))
    (let loop ((i 4)
               (captures captures))
      (unless (null? captures)
        (emit! ctx `(store ,dst ,i ,(binding-value (cdr (assq (car captures) env)))))
        (loop (+ i 1) (cdr captures))))
    dst))


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


;; and/or: jump to the end with the current value when the test
;; (je: false, jne: true) succeeds; the last value is the result
(define (compile-value-logic ctx env exprs empty jump)
  (cond
   ((null? exprs)
    (compile-value ctx env empty))
   ((null? (cdr exprs))
    (compile-value ctx env (car exprs)))
   (else
    (let ((dst (fresh-var ctx))
          (label-end (fresh-label ctx)))
      (let loop ((exprs exprs))
        (emit! ctx `(mov ,dst ,(compile-value ctx env (car exprs))))
        (unless (null? (cdr exprs))
          (emit! ctx `(cmp ,dst (imm #f)))
          (emit! ctx `(,jump ,label-end))
          (loop (cdr exprs))))
      (emit! ctx `(def ,label-end))
      dst))))


(define (compile-tail-logic ctx env exprs empty jump)
  (cond
   ((null? exprs)
    (emit! ctx `(ret (imm ,empty))))
   ((null? (cdr exprs))
    (compile-tail ctx env (car exprs)))
   (else
    (let ((dst (fresh-var ctx))
          (label-end (fresh-label ctx)))
      (let loop ((exprs exprs))
        (if (null? (cdr exprs))
            (compile-tail ctx env (car exprs))
            (begin
              (emit! ctx `(mov ,dst ,(compile-value ctx env (car exprs))))
              (emit! ctx `(cmp ,dst (imm #f)))
              (emit! ctx `(,jump ,label-end))
              (loop (cdr exprs)))))
      (emit! ctx `(def ,label-end))
      (emit! ctx `(ret ,dst))))))


(define (bind-let-vars ctx env bindings)
  (fold-left (lambda (new-env b)
               (acons (car b) (make-local-binding (compile-value ctx env (cadr b))) new-env))
             env
             bindings))


(define (bind-let*-vars ctx env bindings)
  (fold-left (lambda (env b)
               (acons (car b) (make-local-binding (compile-value ctx env (cadr b))) env))
             env
             bindings))


(define (compile-operands ctx env form)
  (let* ((vars (compile-values ctx env form))
         (code (fresh-var ctx)))
    (emit! ctx `(ccall void "check_procedure" ,(car vars) (quad ,(- (length vars) 1))))
    (emit! ctx `(load ,code ,(car vars) 2))
    (cons code vars)))


(define (compile-value-call ctx env form)
  (let ((binding (primitive-operator env form)))
    (if binding
        (compile-primitive-call ctx env binding form)
        (let* ((operands (compile-operands ctx env form))
               (dst (fresh-var ctx)))
          (emit! ctx `(call ,dst ,@operands))
          dst))))


(define (compile-tail-call ctx env form)
  (let ((binding (primitive-operator env form)))
    (if binding
        (emit! ctx `(ret ,(compile-primitive-call ctx env binding form)))
        (emit! ctx `(tailcall ,@(compile-operands ctx env form))))))


;; (define (name arg ...) body ...) => (values name args body)
;; (define name init) => (values name #f (init))
(define (parse-define form)
  (unless (and (non-empty-list? form)
               (eq? (car form) 'define))
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


;; A top-level define: cell index of "scheme_globals", args #f for a value
(define-record-type global-def
  (fields name index args body))


;; A prelude define, proc and code the symbols of its procedure object and
;; function
(define-record-type prelude-def
  (fields name proc code args body))


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
               (make-prelude-def name (prelude-proc-name (+ i 1)) (prelude-fn-name (+ i 1)) args body))))


(define (bind-prelude-defs env defs)
  (append (map (lambda (d)
                 (cons (prelude-def-name d)
                       (make-procedure-binding (prelude-def-proc d) (length (prelude-def-args d)))))
               defs)
          env))


(define (extend-env-with-args env args)
  (let loop ((i 1)
             (args args)
             (env env))
    (if (null? args)
        env
        (loop (+ i 1) (cdr args)
              (acons (car args) (make-local-binding (list 'arg i)) env)))))


;; Load each capture and return a new env
(define (load-captures ctx env captures)
  (let loop ((i 4)
             (captures captures)
             (env env))
    (if (null? captures)
        env
        (let ((dst (fresh-var ctx)))
          (emit! ctx `(load ,dst (self) ,i))
          (loop (+ i 1) (cdr captures) (acons (car captures) (make-local-binding dst) env))))))


(define (function->ir pctx env proc captures args body)
  (let* ((ctx (new-expr-context pctx))
         (env (load-captures ctx env captures)))
    (let-values (((env last) (compile-body ctx (extend-env-with-args env args) body)))
      (compile-tail ctx env last))
    (expr-context->function ctx proc (length args))))


;; Every top-level define is a cell holding #<undef> until scheme_main
;; stores its init, in source order
(define (program->ir env program)
  (when (null? program)
    (error 'program->ir "empty program"))

  (let-values (((forms expr) (split-last program)))
    (let* ((globals (scan-defs forms make-global-def))
           (env (append (map (lambda (g)
                               (cons (global-def-name g)
                                     (make-global-binding (global-def-index g))))
                             globals)
                        env))
           (pctx (new-program-context))
           (n (length globals)))
      (add-entry! pctx (main->ir pctx env globals expr))
      (add-entry! pctx `(cells "scheme_globals" ,n))
      (add-entry! pctx `(data "scheme_nglobals" (quad ,n)))
      (reverse (program-context-entries pctx)))))


(define (main->ir pctx env globals expr)
  (let ((ctx (new-expr-context pctx)))
    (for-each (lambda (g)
                (let ((src (if (global-def-args g)
                               (compile-procedure ctx env (global-def-args g) (global-def-body g))
                               (compile-value ctx env (car (global-def-body g))))))
                  (emit! ctx `(store "scheme_globals" ,(global-def-index g) ,src))))
              globals)
    (compile-tail ctx env expr)
    (expr-context->function ctx "scheme_main" 0)))


;; The global symbols of program->ir
(define program-exports '("scheme_main" "scheme_globals" "scheme_nglobals"))


;; env must already hold the bindings of defs
(define (prelude->ir env defs)
  (let ((pctx (new-program-context)))
    (for-each (lambda (d)
                (let ((args (prelude-def-args d)))
                  (add-entry! pctx (proc-data (prelude-def-proc d) (length args) (prelude-def-code d)))
                  (add-entry! pctx (function->ir pctx env (prelude-def-code d) '() args (prelude-def-body d)))))
              defs)
    (reverse (program-context-entries pctx))))


;; The global symbols of prelude->ir
(define (prelude-exports defs)
  (map prelude-def-proc defs))


;; The primitive binding of the operator of form, or #f
(define (primitive-operator env form)
  (let ((x (and (symbol? (car form)) (assq (car form) env))))
    (and x
         (primitive-binding? (cdr x))
         (cdr x))))


;; Inliners: (inliner ctx args) emits the code and returns dst
(define (inline-eq ctx args)
  (let ((dst (fresh-var ctx))
        (label (fresh-label ctx)))
    (emit! ctx `(mov ,dst (imm #t)))
    (emit! ctx `(cmp ,(car args) ,(cadr args)))
    (emit! ctx `(je ,label))
    (emit! ctx `(mov ,dst (imm #f)))
    (emit! ctx `(def ,label))
    dst))


(define (inline-null ctx args)
  (inline-eq ctx (list (car args) '(imm '()))))


;; Pairs have tag 3 and no header
(define (inline-cons ctx args)
  (let ((dst (fresh-var ctx)))
    (emit! ctx `(alloc ,dst 2 3))
    (emit! ctx `(store ,dst 0 ,(car args)))
    (emit! ctx `(store ,dst 1 ,(cadr args)))
    dst))


;; Inline the primitive or call its C function, returning dst
(define (emit-primitive ctx binding args)
  (let ((inliner (primitive-binding-inliner binding)))
    (if inliner
        (inliner ctx args)
        (let ((dst (fresh-var ctx)))
          (emit! ctx `(ccall ,dst ,(primitive-binding-name binding) ,@args))
          dst))))


(define (compile-primitive-call ctx env binding form)
  (unless (= (length (cdr form)) (procedure-binding-arity binding))
    (error 'compile-primitive-call "wrong number of arguments" form))
  (emit-primitive ctx binding (compile-values ctx env (cdr form))))


;; Function in the Scheme calling convention wrapping the primitive, called
;; through its procedure object
(define (prim-function binding)
  (let* ((arity (procedure-binding-arity binding))
         (ctx (new-expr-context (new-program-context)))
         (args (map (lambda (i) (list 'arg (+ i 1))) (iota arity))))
    (emit! ctx `(ret ,(emit-primitive ctx binding args)))
    (expr-context->function ctx (prim-fn-name (primitive-binding-name binding)) arity)))
