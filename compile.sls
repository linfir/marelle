;; Scheme to low: closure conversion, lambda lifting and renaming;
;; low does the rest (tmps, labels, tail calls, assigned locals)

(library (compile)
  (export compile-scheme)
  (import (chezscheme)
          (utils)
          (compile-parse)
          (compile-vars))


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


  ;; (scheme-name low-name arity)
  (define low-primitives
    '((eq? eq? 2)
      (+ + 2) (- - 2) (* * 2)
      (= = 2) (< < 2) (<= <= 2) (> > 2) (>= >= 2)
      (null? null? 1) (pair? pair? 1)
      (cons cons 2) (car car 1) (cdr cdr 1)
      (not scm-not 1)))


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

     ((special? env expr)
      (case (car expr)
        ((if)
         (let-values (((test then else) (parse-if expr)))
           `(if ,(lower-test prog env test)
                ,(lower-value prog env then)
                ,(lower-value prog env else))))

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
           (lower-set! prog env var value)))))

     ((call? env expr)
      (lower-call prog env expr))

     ((symbol? expr)
      (let ((b (lookup-binding env expr)))
        (cond
         ((global-binding? b) `(global-ref (raw ,(global-binding-index b))))
         ((box-binding? b) `(box-ref ,(local-binding-operand b)))
         ((local-binding? b) (local-binding-operand b))
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
           (primitive-named? env (car expr) 'scm-not))
      `(not ,(lower-test prog env (cadr expr))))

     (else
      (lower-value prog env expr))))


  (define (primitive-named? env x low-name)
    (let ((entry (and (symbol? x) (assq x env))))
      (and entry
           (primitive-binding? (cdr entry))
           (eq? (procedure-binding-proc (cdr entry)) low-name))))


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
       (let-values (((bindings body) (parse-let expr)))
         (let* ((vars (free-vars (bind-scope env (map car bindings)) body))
                (locals (map-in-order (lambda (b) (lower-local prog env (car b) (cadr b) vars))
                                      bindings)))
           `(let ,(map car locals)
              ,@(lower-body prog
                            (append (map cdr locals) env)
                            body lower-last)))))

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


  (define (lift-procedure! prog env def-name name args body)
    (let* ((captures (map (lambda (var) (assq var env))
                          (reverse (captured-vars env args body))))
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


  (define (lower-call prog env form)
    (let ((entry (and (symbol? (car form)) (assq (car form) env)))
          (args (map-in-order (lambda (x) (lower-value prog env x)) (cdr form))))
      (if (and entry (primitive-binding? (cdr entry)))
          (begin
            (unless (= (length args) (primitive-binding-arity (cdr entry)))
              (error 'lower-call "wrong number of arguments" form))
            (cons (procedure-binding-proc (cdr entry)) args))
          (lower-apply prog (lower-value prog env (car form)) args))))


  ;; A def of the right arity is called directly, any other procedure value indirectly
  (define (lower-apply prog op args)
    (let ((arity (and (symbol? op) (hashtable-ref (low-program-arity prog) op #f))))
      (if (and arity (not (= arity (length args))))
          (let ((p (fresh-name prog 'proc)))
            `(let ((,p ,op)) (,p ,@args)))
          (cons op args))))


  ;; The low defs of the prelude and the program, after (globals n)
  (define (program->low prelude program)
    (when (null? program)
      (error 'program->low "empty program"))
    (let* ((prog (make-low-program))
           (prelude-defs (scan-prelude prelude))
           (prelude-names (map-in-order (lambda (d) (fresh-name prog (prelude-def-name d)))
                                        prelude-defs))
           (env (append (map (lambda (p)
                               (cons (car p) (make-primitive-binding (cadr p) (caddr p))))
                             low-primitives)
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


  (define (compile-scheme prelude program)
    (parameterize ((print-brackets #f))
      (for-each pretty-print (program->low prelude program)))))
