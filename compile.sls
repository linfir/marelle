;; Scheme to low: assignment conversion (boxes), closure conversion, lambda lifting and renaming;
;; low does the rest (tmps, labels, tail calls, assigned locals)

(library (compile)
  (export compile-scheme compile-runtime)
  (import (chezscheme)
          (utils)
          (only (low-parse) binary-ops tests keywords)
          (compile-parse)
          (compile-vars))


  ;; True while lowering runtime.ss, which may use strings, named lets and low operations
  (define runtime-code? (make-parameter #f))


  ;; The low operations runtime.ss may call
  (define low-ops
    (append (map car binary-ops)
            (map car tests)
            '(raw as-raw as-val syscall load store load8 store8 copy-words fill-words
                  alloc self address)))


  ;; A captured local is either boxed or never assigned
  (define (capture-binding operand binding)
    (if (box-binding? binding)
        (make-box-binding operand)
        (make-local-binding operand)))


  ;; names  counter of the fresh names
  ;; defs   the lifted defs, newest first
  ;; arity  arity of each def name, for known calls
  (define-record-type low-program
    (fields names (mutable defs) arity)
    (protocol
     (lambda (new)
       (lambda ()
         (new (make-counter) '() (make-eq-hashtable))))))


  ;; Every local and def gets a name x.N, unique in the program
  (define (fresh-name prog x)
    (string->symbol (format "~a.~a" x ((low-program-names prog)))))


  (define (add-def! prog def)
    (low-program-defs-set! prog (cons def (low-program-defs prog))))


  (define (lookup-binding env x)
    (let ((entry (assq x env)))
      (unless entry
        (error 'lookup-binding "unknown symbol" x))
      (cdr entry)))


  (define (lower-immediate x)
    (if (eq? x undef) 'undef x))


  (define (lower-value prog env expr)
    (cond
     ((immediate? expr)
      (lower-immediate expr))

     ((number? expr)
      (error 'lower-value "unsupported number" expr))

     ((and (string? expr) (runtime-code?))
      expr)

     ((special? env expr)
      (case (car expr)
        ((if)
         (let-values (((test then else) (parse-if expr)))
           `(if ,(lower-test prog env test)
                ,(lower-value prog env then)
                ,(lower-value prog env else))))

        ((cond)
         (lower-cond prog env (parse-cond expr)))

        ((and)
         (lower-and prog env (cdr expr)))

        ((or)
         (lower-or prog env (cdr expr)))

        ((let let* begin)
         (lower-scope prog env expr lower-value))

        ((lambda)
         (let-values (((args body) (parse-lambda expr)))
           (lower-procedure prog env #f args body)))

        ((set!)
         (let-values (((var value) (parse-set! expr)))
           (lower-set! prog env var value)))

        ((when unless)
         (lower-when prog env expr))))

     ((call? env expr)
      (lower-call prog env expr))

     ((symbol? expr)
      (let ((b (lookup-binding env expr)))
        (cond
         ((global-binding? b) `(global-ref (raw ,(global-binding-index b))))
         ((box-binding? b) `(box-ref ,(local-binding-operand b)))
         ((local-binding? b) (local-binding-operand b))
         ((constant-binding? b) (constant-binding-operand b))
         ((or (low-op-binding? b) (loop-binding? b))
          (error 'lower-value "low operation or named let used as a value" expr))
         (else (procedure-binding-proc b)))))

     (else
      (error 'lower-value "unknown form" expr))))


  ;; expr as the test of an if: and, or and not stay low's, which branch
  (define (lower-test prog env expr)
    (cond
     ((and (special? env expr) (memq (car expr) '(and or)))
      (cons (car expr) (map-in-order (lambda (x) (lower-test prog env x)) (cdr expr))))

     ((and (special? env expr) (memq (car expr) '(let let* begin)))
      (lower-scope prog env expr lower-test))

     ((and (call? env expr)
           (= (length expr) 2)
           (eq? (car expr) 'not)
           (primitive-binding? (operator-binding env 'not)))
      `(not ,(lower-test prog env (cadr expr))))

     (else
      (lower-value prog env expr))))


  (define (operator-binding env x)
    (let ((entry (and (symbol? x) (assq x env))))
      (and entry (cdr entry))))


  ;; Low's when and unless give undef and allow a void body, so only runtime.ss uses them
  (define (lower-when prog env expr)
    (unless (>= (length expr) 3)
      (error 'lower-when "invalid when or unless" expr))
    (let ((test (lower-test prog env (cadr expr)))
          (body (map-in-order (lambda (x) (lower-value prog env x)) (cddr expr))))
      (cond
       ((runtime-code?) `(,(car expr) ,test ,@body))
       ((eq? (car expr) 'when) `(if ,test (begin ,@body) undef))
       (else `(if ,test undef (begin ,@body))))))


  ;; Each clause is (test expr ...), (test), (test => f), or a last (else expr ...)
  (define (lower-cond prog env clauses)
    (if (null? clauses)
        'undef
        (let ((test (caar clauses))
              (body (cdar clauses))
              (rest (cdr clauses)))
          (cond
           ((auxiliary? env test 'else)
            (unless (and (null? rest) (pair? body))
              (error 'lower-cond "invalid else clause" (car clauses)))
            (lower-sequence prog env body))
           ((null? body)
            (let ((t (fresh-name prog 'cond)))
              `(let ((,t ,(lower-value prog env test)))
                 (if ,t ,t ,(lower-cond prog env rest)))))
           ((auxiliary? env (car body) '=>)
            (unless (= (length body) 2)
              (error 'lower-cond "invalid => clause" (car clauses)))
            (let ((t (fresh-name prog 'cond))
                  (x (gensym)))
              `(let ((,t ,(lower-value prog env test)))
                 (if ,t
                     ,(lower-call prog (acons x (make-local-binding t) env) (list (cadr body) x))
                     ,(lower-cond prog env rest)))))
           (else
            `(if ,(lower-test prog env test)
                 ,(lower-sequence prog env body)
                 ,(lower-cond prog env rest)))))))


  ;; else and => are keywords unless bound
  (define (auxiliary? env x keyword)
    (and (eq? x keyword)
         (not (assq keyword env))))


  (define (lower-sequence prog env exprs)
    (let ((xs (lower-body prog env exprs lower-value)))
      (if (null? (cdr xs)) (car xs) `(begin ,@xs))))


  ;; The value of the deciding operand
  (define (lower-and prog env exprs)
    (cond
     ((null? exprs) #t)
     ((null? (cdr exprs)) (lower-value prog env (car exprs)))
     (else `(if ,(lower-test prog env (car exprs))
                ,(lower-and prog env (cdr exprs))
                #f))))


  (define (lower-or prog env exprs)
    (cond
     ((null? exprs) #f)
     ((null? (cdr exprs)) (lower-value prog env (car exprs)))
     (else
      (let ((t (fresh-name prog 'or)))
        `(let ((,t ,(lower-value prog env (car exprs))))
           (if ,t ,t ,(lower-or prog env (cdr exprs))))))))


  ;; let, let* and begin, the last body expression by (lower-last prog env x)
  (define (lower-scope prog env expr lower-last)
    (case (car expr)
      ((let)
       (if (named-let? expr)
           (lower-named-let prog env expr lower-last)
           (lower-let prog env expr lower-last)))

      ((let*)
       (let-values (((bindings body) (parse-let* expr)))
         (let loop ((bindings bindings)
                    (env env)
                    (acc '()))
           (if (null? bindings)
               `(let* ,(reverse acc) ,@(lower-body prog env body lower-last))
               (let* ((var (caar bindings))
                      (vars (walk-let* (bind-scope env (list var)) (cdr bindings) body no-vars))
                      (local (lower-local prog env var (cadar bindings) vars)))
                 (loop (cdr bindings) (cons (cdr local) env) (cons (car local) acc)))))))

      ((begin)
       `(begin ,@(lower-body prog env (parse-begin expr) lower-last)))))


  (define (lower-let prog env expr lower-last)
    (let-values (((bindings body) (parse-let expr)))
      (let* ((vars (free-vars (bind-scope env (map car bindings)) body))
             (locals (map-in-order (lambda (b) (lower-local prog env (car b) (cadr b) vars))
                                   bindings)))
        `(let ,(map car locals)
           ,@(lower-body prog
                         (append (map cdr locals) env)
                         body lower-last)))))


  ;; A named let of runtime.ss: a low loop, so a call to its name must be in exit position
  (define (lower-named-let prog env expr lower-last)
    (unless (runtime-code?)
      (error 'lower-named-let "named let is only supported in runtime.ss" expr))
    (let-values (((name bindings body) (parse-named-let expr)))
      (let* ((vars (map car bindings))
             (free (free-vars (bind-scope env (cons name vars)) body))
             (inits (map-in-order (lambda (b) (lower-value prog env (cadr b))) bindings))
             (loop (fresh-name prog name))
             (names (map-in-order (lambda (v) (fresh-name prog v)) vars)))
        (for-each (lambda (v)
                    (when (boxed? v free)
                      (error 'lower-named-let "assigned and captured loop variable" v)))
                  vars)
        `(let ,loop ,(map list names inits)
              ,@(lower-body prog
                            (append (map (lambda (v n) (cons v (make-local-binding n))) vars names)
                                    (acons name (make-loop-binding loop (length vars)) env))
                            body lower-last)))))


  ;; A let variable var bound to init: ((name init) . (var . binding))
  (define (lower-local prog env var init vars)
    (let* ((name (fresh-name prog var))
           (init (lower-named prog env var init)))
      (if (boxed? var vars)
          (cons `(,name (make-box ,init)) (cons var (make-box-binding name)))
          (cons `(,name ,init) (cons var (make-local-binding name))))))


  (define (lower-body prog env exprs lower-last)
    (let-values (((init last) (split-last exprs)))
      (append (map-in-order (lambda (x) (lower-value prog env x)) init)
              (list (lower-last prog env last)))))


  ;; expr, naming it name if it is a lambda
  (define (lower-named prog env name expr)
    (if (and (special? env expr)
             (eq? (car expr) 'lambda))
        (let-values (((args body) (parse-lambda expr)))
          (lower-procedure prog env name args body))
        (lower-value prog env expr)))


  ;; Lifts the procedure into a def and returns its value:
  ;; the def name, or a closure filled with the captures
  (define (lower-procedure prog env name args body)
    (let ((def-name (fresh-name prog (or name 'lambda))))
      (lift-procedure! prog env def-name name args body)))


  (define (check-captures captures)
    (for-each (lambda (c)
                (when (raw-binding? (cdr c))
                  (error 'check-captures "captured raw variable" (car c))))
              captures)
    captures)


  (define (lift-procedure! prog env def-name name args body)
    (let* ((captures (check-captures (map (lambda (var) (assq var env))
                                          (reverse (captured-vars env args body)))))
           (outer-env (filter (lambda (b) (not (local-binding? (cdr b)))) env))
           (capture-names (map-in-order (lambda (c) (fresh-name prog (car c))) captures))
           (inner-env (append (map (lambda (c name) (cons (car c) (capture-binding name (cdr c))))
                                   captures capture-names)
                              outer-env))
           (params (map-in-order (lambda (x) (fresh-name prog x)) args))
           (vars (free-vars (bind-scope inner-env args) body))
           ;; (param . box name) of the boxed parameters
           (boxes (map-in-order (lambda (x) (cons (cdr x) (fresh-name prog (car x))))
                                (filter (lambda (x) (boxed? (car x) vars)) (map cons args params))))
           (body-env (append (map (lambda (x p)
                                    (cons x (let ((box (assq p boxes)))
                                              (if box
                                                  (make-box-binding (cdr box))
                                                  (make-local-binding p)))))
                                  args params)
                             inner-env))
           (body (lower-body prog body-env body lower-value))
           (body (if (null? boxes)
                     body
                     `((let ,(map (lambda (box) `(,(cdr box) (make-box ,(car box)))) boxes)
                         ,@body))))
           (body (if (null? captures)
                     body
                     `((let ,(map (lambda (name i) `(,name (closure-ref (raw ,i))))
                                  capture-names (iota (length captures)))
                         ,@body))))
           (attrs `(,@(if name `((:as ,name)) '(:anonymous))
                    ,@(if (null? captures) '() `((:captures ,(length captures)))))))
      (hashtable-set! (low-program-arity prog) def-name (length args))
      (add-def! prog `(def ,def-name ,(map (lambda (p) `(val ,p)) params) -> (val) ,attrs
                           ,@body))
      (if (null? captures)
          def-name
          (let ((c (fresh-name prog 'closure)))
            `(let ((,c (alloc-closure ,def-name)))
               ,@(map (lambda (capture i)
                        `(closure-set! ,c (raw ,i) ,(local-binding-operand (cdr capture))))
                      captures (iota (length captures)))
               (as-val ,c))))))


  (define (lower-set! prog env var expr)
    (let ((src (lower-named prog env var expr))
          (b (lookup-binding env var)))
      (cond
       ((box-binding? b)
        `(begin (box-set! ,(local-binding-operand b) ,src) undef))
       ((local-binding? b)
        `(begin (set! ,(local-binding-operand b) ,src) undef))
       ((global-binding? b)
        `(begin (global-set! (raw ,(global-binding-index b)) ,src) undef))
       (else
        (error 'lower-set! "set! of a prelude procedure or primitive" var)))))


  ;; A primitive, low operation or named let is called by name, (raw n) is kept as is
  (define (lower-call prog env form)
    (let ((b (operator-binding env (car form))))
      (if (and (low-op-binding? b) (eq? (low-op-binding-name b) 'raw))
          form
          (let ((args (map-in-order (lambda (x) (lower-value prog env x)) (cdr form))))
            (define (check-arity n)
              (unless (= (length args) n)
                (error 'lower-call "wrong number of arguments" form)))
            (cond
             ((low-op-binding? b)
              (cons (low-op-binding-name b) args))
             ((primitive-binding? b)
              (check-arity (primitive-binding-arity b))
              (cons (procedure-binding-proc b) args))
             ((loop-binding? b)
              (check-arity (loop-binding-arity b))
              (cons (loop-binding-operand b) args))
             (else
              (lower-apply prog (lower-value prog env (car form)) args)))))))


  ;; A def of the right arity is called directly, any other procedure value indirectly
  (define (lower-apply prog op args)
    (let ((arity (and (symbol? op) (hashtable-ref (low-program-arity prog) op #f))))
      (if (and arity (not (= arity (length args))))
          (let ((p (fresh-name prog 'proc)))
            `(let ((,p ,op)) (,p ,@args)))
          (cons op args))))


  (define (runtime-item-name x)
    (if (runtime-def? x) (runtime-def-name x) (runtime-data-name x)))


  ;; Runtime names stay, for the stubs and the frontend output, unless low reserves them
  (define (runtime-low-name prog x)
    (if (memq x keywords) (fresh-name prog x) x))


  (define (result->low k)
    (case k
      ((val raw) (list k))
      ((void) '())
      ((never) '!)))


  ;; Lowers the defs and data of runtime.ss, returns the env of its public defs
  (define (lower-runtime! prog runtime)
    (let* ((items (scan-runtime runtime))
           (names (map-in-order (lambda (x) (runtime-low-name prog (runtime-item-name x))) items))
           (bindings (map (lambda (x name)
                            (cons (runtime-item-name x)
                                  (if (runtime-def? x)
                                      (make-primitive-binding name (length (runtime-def-args x)))
                                      (make-constant-binding name))))
                          items names))
           (env (append bindings
                        (map (lambda (op) (cons op (make-low-op-binding op))) low-ops)
                        (list (cons 'undef (make-constant-binding 'undef))))))
      (parameterize ((runtime-code? #t))
        (for-each (lambda (x name)
                    (if (runtime-def? x)
                        (lower-runtime-def! prog env x name)
                        (add-def! prog `(data ,name ,(runtime-data-nwords x)
                                              ,(runtime-data-attrs x)))))
                  items names))
      (fold-right (lambda (x b acc)
                    (if (and (runtime-def? x) (memq ':public (runtime-def-attrs x)))
                        (cons b acc)
                        acc))
                  '() items bindings)))


  (define (lower-runtime-def! prog env d name)
    (let* ((args (runtime-def-args d))
           (params (map-in-order (lambda (x) (fresh-name prog x)) args))
           (body (runtime-def-body d))
           (vars (free-vars (bind-scope env args) body))
           (body-env (append (map (lambda (x p k)
                                    (when (boxed? x vars)
                                      (error 'lower-runtime-def!
                                             "assigned and captured parameter" x))
                                    (cons x (if (eq? k 'raw)
                                                (make-raw-binding p)
                                                (make-local-binding p))))
                                  args params (runtime-def-kinds d))
                             env))
           (attrs (if (eq? name (runtime-def-name d))
                      (runtime-def-attrs d)
                      `(,@(runtime-def-attrs d) (:as ,(runtime-def-name d))))))
      (add-def! prog `(def ,name ,(map list (runtime-def-kinds d) params)
                           -> ,(result->low (runtime-def-result d)) ,attrs
                           ,@(lower-body prog body-env body lower-value)))))


  ;; The low defs and data of runtime.ss, the prelude and the program, after (globals n)
  (define (program->low runtime prelude program)
    (when (null? program)
      (error 'program->low "empty program"))
    (let* ((prog (make-low-program))
           (public (lower-runtime! prog runtime))
           (prelude-defs (scan-prelude prelude))
           (prelude-names (map-in-order (lambda (d) (fresh-name prog (prelude-def-name d)))
                                        prelude-defs))
           (env (append public
                        (map (lambda (d name)
                               (cons (prelude-def-name d) (make-procedure-binding name)))
                             prelude-defs prelude-names))))
      (for-each (lambda (d name)
                  (hashtable-set! (low-program-arity prog) name (length (prelude-def-args d))))
                prelude-defs prelude-names)
      (for-each (lambda (d name)
                  (lift-procedure! prog env name (prelude-def-name d)
                                   (prelude-def-args d) (prelude-def-body d)))
                prelude-defs prelude-names)
      ;; A program ending with a define evaluates to undef
      (let-values (((forms expr) (if (define-form? (car (last-pair program)))
                                     (values program undef)
                                     (split-last program))))
        (let* ((globals (scan-defs forms make-global-def))
               (env (append (map (lambda (g)
                                   (cons (global-def-name g)
                                         (make-global-binding (global-def-index g))))
                                 globals)
                            env))
               (inits (map-in-order (lambda (g)
                                      `(global-set! (raw ,(global-def-index g))
                                                    ,(lower-global prog env g)))
                                    globals))
               (main `(def main () -> (val) () ,@inits ,(lower-value prog env expr))))
          `((globals ,(length globals))
            ,@(reverse (low-program-defs prog))
            ,main)))))


  (define (lower-global prog env g)
    (if (global-def-args g)
        (lower-procedure prog env (global-def-name g) (global-def-args g) (global-def-body g))
        (lower-named prog env (global-def-name g) (car (global-def-body g)))))


  (define (compile-scheme runtime prelude program)
    (parameterize ((print-brackets #f))
      (for-each pretty-print (program->low runtime prelude program))))


  ;; The low defs and data of runtime.ss alone, for low programs
  (define (compile-runtime runtime)
    (let ((prog (make-low-program)))
      (lower-runtime! prog runtime)
      (parameterize ((print-brackets #f))
        (for-each pretty-print (reverse (low-program-defs prog)))))))
