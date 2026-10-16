(library (compile-vars)
  (export make-global-binding global-binding? global-binding-index
          make-local-binding local-binding? local-binding-operand
          make-box-binding box-binding?
          make-procedure-binding procedure-binding-proc
          make-primitive-binding primitive-binding? primitive-binding-arity
          no-vars free-vars walk-let* bind-scope captured-vars boxed?)
  (import (chezscheme)
          (utils)
          (compile-parse))


  (define-record-type global-binding
    (fields index))


  (define-record-type local-binding
    (fields operand))


  (define-record-type box-binding
    (parent local-binding))


  (define-record-type bound-binding)


  (define-record-type procedure-binding
    (fields proc))


  (define-record-type primitive-binding
    (parent procedure-binding)
    (fields arity))


  ;; Free variables of exprs, not bound to a bound-binding in env:
  ;; the referenced, the assigned, and the ones used in a lambda
  (define-record-type var-sets
    (fields refs sets caps))


  (define no-vars (make-var-sets '() '() '()))


  (define (free-vars env exprs)
    (walk-exprs env exprs no-vars))


  (define (walk-exprs env exprs acc)
    (fold-left (lambda (acc x) (walk-expr env x acc)) acc exprs))


  (define (walk-expr env expr acc)
    (cond
     ((symbol? expr)
      (add-free-var env expr acc #f))

     ((special? env expr)
      (case (car expr)
        ((let)
         (let-values (((bindings body) (parse-let expr)))
           (walk-let env bindings body acc)))

        ((let*)
         (let-values (((bindings body) (parse-let* expr)))
           (walk-let* env bindings body acc)))

        ((lambda)
         (let-values (((args body) (parse-lambda expr)))
           (let ((inner (walk-exprs (bind-inside env args) body no-vars)))
             (make-var-sets (union (var-sets-refs inner) (var-sets-refs acc))
                            (union (var-sets-sets inner) (var-sets-sets acc))
                            (union (var-sets-refs inner) (var-sets-caps acc))))))

        ((set!)
         (let-values (((var value) (parse-set! expr)))
           (add-free-var env var (walk-expr env value acc) #t)))

        (else
         (walk-exprs env (cdr expr) acc))))

     ((list? expr)
      (walk-exprs env expr acc))

     (else acc)))


  (define (walk-let env bindings body acc)
    (walk-exprs (bind-inside env (map car bindings))
                body
                (walk-exprs env (map cadr bindings) acc)))


  (define (walk-let* env bindings body acc)
    (if (null? bindings)
        (walk-exprs env body acc)
        (walk-let* (bind-inside env (list (caar bindings)))
                   (cdr bindings)
                   body
                   (walk-expr env (cadar bindings) acc))))


  (define (add-free-var env var acc set?)
    (let ((x (assq var env)))
      (if (and x (bound-binding? (cdr x)))
          acc
          (make-var-sets (adjoin var (var-sets-refs acc))
                         (if set? (adjoin var (var-sets-sets acc)) (var-sets-sets acc))
                         (var-sets-caps acc)))))


  ;; xs added to ys, as if adjoined in the order they were found
  (define (union xs ys)
    (fold-right adjoin ys xs))


  (define (bind-inside env names)
    (append (map (lambda (x) (cons x (make-bound-binding))) names) env))


  (define (bind-scope env names)
    (append (map (lambda (x) (cons x #f)) names) env))


  (define (captured-vars env args body)
    (filter (lambda (var)
              (let ((x (assq var env)))
                (and x (local-binding? (cdr x)))))
            (var-sets-refs (free-vars (bind-inside env args) body))))


  (define (boxed? var vars)
    (and (memq var (var-sets-sets vars))
         (memq var (var-sets-caps vars))
         #t)))
