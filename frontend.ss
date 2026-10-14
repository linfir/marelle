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




(define-record-type global-binding
  (fields index))


(define-record-type local-binding
  (fields operand assigned?))


(define-record-type box-binding
  (parent local-binding))


(define-record-type bound-binding)


(define-record-type procedure-binding
  (fields proc arity))


(define-record-type primitive-binding
  (parent procedure-binding))


(define (binding-value binding)
  (if (local-binding? binding)
      (local-binding-operand binding)
      (procedure-binding-proc binding)))


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


;; compile expr in value position
;;   env is an alist of pairs (symbol . binding)
;;   code is accumulated in ctx
;; returns dst or throw an error
(define (call? env expr)
  (and (non-empty-list? expr)
       (not (immediate? expr))
       (not (special? env expr))))


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
       #t))


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
               (make-prelude-def name args body))))


;; A captured local is either boxed or never assigned
(define (capture-binding operand binding)
  (if (box-binding? binding)
      (make-box-binding operand #t)
      (make-local-binding operand #f)))
