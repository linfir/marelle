;; The front pass: every local variable renamed to a fresh gensym, whose pretty name is the
;; original, and every special form headed by its marker, so a plain symbol at the head of a
;; form is a call.
;; The derived forms are expanded: cond, case, let*, letrec (as letrec*), do, let-values,
;; let*-values, parameterize, the bodies' defines, define-values and define-record-type, and
;; when and unless outside unsafe defines.
;; The core forms left are if, begin, set!, lambda, let, named let, letrec* (for the letrec
;; pass), and, or, eq? (from case), and when and unless in unsafe defines.

(library (front)
  (export front-def expand-top-define-values expand-top-define-record-type)
  (import (chezscheme)
          (utils)
          (parse))


  ;; True in an unsafe define, where when and unless stay
  (define unsafe? (make-parameter #f))


  (define (fresh-names names)
    (map (lambda (x) (gensym (symbol->string x))) names))


  ;; A local's entry holds its fresh name, any other entry a binding record
  (define (bind env names fresh)
    (append (map cons names fresh) env))


  ;; (names . body): the names of a def's parameters, renamed, and its body, in env
  (define (front-def env names body unsafe)
    (parameterize ((unsafe? unsafe))
      (let ((fresh (fresh-names names)))
        (cons fresh (front-body (bind env names fresh) body)))))


  (define (front-body env body)
    (front-exprs env (expand-body env body)))


  (define (front-exprs env exprs)
    (map (lambda (x) (front-expr env x)) exprs))


  (define (front-expr env x)
    (cond
     ((symbol? x)
      (let ((entry (assq x env)))
        (if (and entry (symbol? (cdr entry))) (cdr entry) x)))
     ((or (vector? x) (bytevector? x))
      (front-quoted x))
     ((or (immediate? x) (not (non-empty-list? x)))
      x)
     ((quoted-datum? x)
      (front-quoted (cadr x)))
     (else
      (let ((keyword (source-keyword env x)))
        (case keyword
          ((#f)
           (front-exprs env x))

          ((lambda)
           (let-values (((formals body) (parse-lambda x)))
             (let* ((names (formals-names formals))
                    (fresh (fresh-names names)))
               `(,(marker 'lambda) ,(rename-formals formals fresh)
                 ,@(front-body (bind env names fresh) body)))))

          ((let)
           (if (named-let? x)
               (let-values (((name bindings body) (parse-named-let x)))
                 (let* ((vars (map car bindings))
                        (fresh-name (car (fresh-names (list name))))
                        (fresh (fresh-names vars)))
                   ;; The variables shadow the name
                   `(,(marker 'let) ,fresh-name
                     ,(map list fresh (front-exprs env (map cadr bindings)))
                     ,@(front-body (bind (bind env (list name) (list fresh-name)) vars fresh)
                                   body))))
               (let-values (((bindings body) (parse-let x)))
                 (let* ((vars (map car bindings))
                        (fresh (fresh-names vars)))
                   `(,(marker 'let) ,(map list fresh (front-exprs env (map cadr bindings)))
                     ,@(front-body (bind env vars fresh) body))))))

          ((let*)
           (front-expr env (expand-let* x)))

          ((letrec letrec*)
           (let-values (((bindings body) (parse-letrec x)))
             (let* ((vars (map car bindings))
                    (fresh (fresh-names vars))
                    (inner (bind env vars fresh)))
               `(,(marker 'letrec*) ,(map list fresh (front-exprs inner (map cadr bindings)))
                 ,@(front-body inner body)))))

          ((cond)
           (front-expr env (expand-cond env (parse-cond x))))

          ((case)
           (front-expr env (expand-case env x)))

          ((do)
           (front-expr env (expand-do x)))

          ((case-lambda)
           (front-expr env (expand-case-lambda x)))

          ((parameterize)
           (front-expr env (expand-parameterize x)))

          ((quasiquote)
           (unless (= (length x) 2)
             (error 'front-expr "invalid quasiquote" x))
           (front-expr env (expand-quasiquote env (cadr x))))

          ((let-values)
           (front-expr env (expand-let-values x)))

          ((let*-values)
           (front-expr env (expand-let*-values x)))

          ((core)
           x)

          ((when unless)
           (if (unsafe?)
               `(,(marker keyword) ,@(front-exprs env (cdr x)))
               (front-expr env (expand-when keyword x))))

          ((define define-values define-record-type)
           (error 'front-expr "invalid context for definition" x))

          (else
           `(,(marker keyword) ,@(front-exprs env (cdr x)))))))))


  ;; A body with its leading defines, also inside begins, as a letrec* around the rest;
  ;; a later define is an error
  (define (expand-body env body)
    (let loop ((forms body)
               (bindings '()))
      (case (and (pair? forms) (source-keyword env (car forms)))
        ((define)
         (let-values (((name args init) (parse-define (car forms))))
           (loop (cdr forms)
                 (cons (list name (if args `(,(marker 'lambda) ,args ,@init) (car init)))
                       bindings))))
        ;; The values in a list t, then each variable bound to its element
        ((define-values)
         (let-values (((formals expr) (parse-define-values (car forms))))
           (let* ((names (formals-names formals))
                  (copies (fresh-names names))
                  (t (gensym "values")))
             (loop (cdr forms)
                   (append (reverse (map (lambda (x i)
                                           (list x `(,(core-ref 'car) ,(core-cdrs i t))))
                                         names
                                         (iota (length names))))
                           (list (list t (call-with-values-form
                                          expr (rename-formals formals copies)
                                          (list `(,(core-ref 'list) ,@copies)))))
                           bindings)))))
        ((define-record-type)
         (loop (cdr forms) (append (reverse (record-type-bindings (car forms))) bindings)))
        ((begin)
         (loop (append (cdar forms) (cdr forms)) bindings))
        (else
         (cond
          ((null? forms) (error 'expand-body "no expression in body" body))
          ((null? bindings) forms)
          (else `((,(marker 'letrec*) ,(reverse bindings) ,@forms))))))))


  (define (core-cdrs n x)
    (if (= n 0) x `(,(core-ref 'cdr) ,(core-cdrs (- n 1) x))))


  ;; At the top level: a define of undef per variable, then a call-with-values setting them
  (define (expand-top-define-values form)
    (let-values (((formals expr) (parse-define-values form)))
      (let* ((names (formals-names formals))
             (copies (fresh-names names)))
        `(,@(map (lambda (x) `(define ,x ,undef)) names)
          ,(call-with-values-form expr (rename-formals formals copies)
                                  `(,@(map (lambda (x c) `(,(marker 'set!) ,x ,c)) names copies)
                                    ,undef))))))


  ;; The type, made when the form is evaluated, then make-name, name?, and name-f and
  ;; name-f-set! per field, which check their record's type
  (define (record-type-bindings form)
    (let-values (((name fields) (parse-define-record-type form)))
      (let ((type (gensym (symbol->string name)))
            (indices (iota (length fields))))
        (define (named . parts)
          (string->symbol
           (apply string-append (map (lambda (x) (if (symbol? x) (symbol->string x) x)) parts))))
        (define (checked who r value)
          `(,(marker 'if) (,(core-ref 'record-of?) ,r ,type)
            ,value
            (,(core-ref 'record-error) ,(make-literal who) ,type ,r)))
        (define (field-bindings field i)
          (let ((get (named name "-" (car field)))
                (r (gensym "r"))
                (x (gensym "x")))
            (cons `(,get (,(marker 'lambda) (,r)
                          ,(checked get r `(,(core-ref 'record-field) ,r ,i))))
                  (if (cdr field)
                      (let ((set (named get "-set!")))
                        `((,set (,(marker 'lambda) (,r ,x)
                                 ,(checked set r `(,(core-ref 'record-field-set!) ,r ,i ,x))))))
                      '()))))
        (let ((args (fresh-names (map car fields)))
              (r (gensym "r"))
              (x (gensym "x")))
          `((,type (,(core-ref 'make-record-type) ,(make-literal name)))
            (,(named "make-" name)
             (,(marker 'lambda) ,args
              (,(marker 'let) ((,r (,(core-ref 'make-record) ,type ,(length fields))))
               ,@(map (lambda (a i) `(,(core-ref 'record-field-set!) ,r ,i ,a)) args indices)
               ,r)))
            (,(named name "?")
             (,(marker 'lambda) (,x) (,(core-ref 'record-of?) ,x ,type)))
            ,@(apply append (map field-bindings fields indices)))))))


  (define (expand-top-define-record-type form)
    (map (lambda (b) `(define ,@b)) (record-type-bindings form)))


  (define (call-with-values-form expr formals body)
    `(,(core-ref 'call-with-values) (,(marker 'lambda) () ,expr) (,(marker 'lambda) ,formals ,@body)))


  ;; Each init evaluated in the outer env, its consumer taking copies of the formals, then a
  ;; let binding the variables around the body; one clause binds them directly
  (define (expand-let-values x)
    (let-values (((bindings body) (parse-let-values x #f)))
      (if (and (pair? bindings) (null? (cdr bindings)))
          (call-with-values-form (cadar bindings) (caar bindings) body)
          (let* ((names (map (lambda (b) (formals-names (car b))) bindings))
                 (copies (map fresh-names names)))
            (let loop ((bindings bindings)
                       (cs copies))
              (if (null? bindings)
                  `(,(marker 'let) ,(map list (apply append names) (apply append copies)) ,@body)
                  (call-with-values-form (cadar bindings)
                                         (rename-formals (caar bindings) (car cs))
                                         (list (loop (cdr bindings) (cdr cs))))))))))


  (define (expand-let*-values x)
    (let-values (((bindings body) (parse-let-values x #t)))
      (if (null? bindings)
          `(,(marker 'let) () ,@body)
          (let loop ((bindings bindings))
            (call-with-values-form (cadar bindings) (caar bindings)
                                   (if (null? (cdr bindings))
                                       body
                                       (list (loop (cdr bindings)))))))))


  (define (expand-let* x)
    (let-values (((bindings body) (parse-let* x)))
      (let loop ((bindings bindings))
        (if (or (null? bindings) (null? (cdr bindings)))
            `(,(marker 'let) ,bindings ,@body)
            `(,(marker 'let) (,(car bindings)) ,(loop (cdr bindings)))))))


  ;; A named let loop; a variable without a step keeps its value, and the value is undef when
  ;; the test has no expression
  (define (expand-do x)
    (let-values (((specs test exprs commands) (parse-do x)))
      (let ((loop (gensym "do")))
        `(,(marker 'let) ,loop ,(map (lambda (s) (list (car s) (cadr s))) specs)
          (,(marker 'if) ,test
           ,(if (null? exprs) undef `(,(marker 'begin) ,@exprs))
           (,(marker 'begin)
            ,@commands
            (,loop ,@(map (lambda (s) (if (null? (cddr s)) (car s) (caddr s))) specs))))))))


  ;; One variadic lambda: the first clause taking the argument count binds its formals from
  ;; the list of the arguments
  (define (expand-case-lambda x)
    (let ((args (gensym "args"))
          (n (gensym "n")))
      (define (clause c)
        (unless (and (list? c) (>= (length c) 2))
          (error 'expand-case-lambda "invalid case-lambda clause" c))
        (validate-parameters (car c))
        (let loop ((formals (car c)) (tail args) (k 0) (bindings '()))
          (cond
           ((pair? formals)
            (loop (cdr formals) `(,(core-ref 'cdr) ,tail) (+ k 1)
                  (cons (list (car formals) `(,(core-ref 'car) ,tail)) bindings)))
           (else
            (values (if (null? formals)
                        `(,(marker 'eq?) ,n ,k)
                        `(,(core-ref '>=) ,n ,k))
                    `(,(marker 'let) ,(reverse (if (null? formals)
                                                   bindings
                                                   (cons (list formals tail) bindings)))
                      ,@(cdr c)))))))
      `(,(marker 'lambda) ,args
        (,(marker 'let) ((,n (,(core-ref 'length) ,args)))
         ,(let loop ((clauses (cdr x)))
            (if (null? clauses)
                `(,(core-ref 'case-lambda-error) ,args)
                (let-values (((test body) (clause (car clauses))))
                  `(,(marker 'if) ,test ,body ,(loop (cdr clauses))))))))))


  ;; Each parameter set by calling it, the body run, then the old values set back.
  ;; Nothing exits non-locally, so no dynamic-wind.
  (define (expand-parameterize x)
    (let-values (((bindings body) (parse-parameterize x)))
      (let ((ps (map (lambda (b) (gensym "p")) bindings))
            (vs (map (lambda (b) (gensym "v")) bindings))
            (olds (map (lambda (b) (gensym "old")) bindings))
            (result (gensym "result")))
        `(,(marker 'let) (,@(map (lambda (p b) (list p (car b))) ps bindings)
                          ,@(map (lambda (v b) (list v (cadr b))) vs bindings))
          (,(marker 'let) ,(map (lambda (old p) `(,old (,p))) olds ps)
           ,@(map list ps vs)
           (,(marker 'let) ((,result (,(marker 'let) () ,@body)))
            ,@(map list ps olds)
            ,result))))))


  ;; Their tails stay in tail position
  (define (expand-when keyword x)
    (unless (>= (length x) 3)
      (error 'expand-when "invalid when or unless" x))
    (let ((body `(,(marker 'begin) ,@(cddr x))))
      (if (eq? keyword 'when)
          `(,(marker 'if) ,(cadr x) ,body ,undef)
          `(,(marker 'if) ,(cadr x) ,undef ,body))))


  ;; Each clause is (test expr ...), (test), (test => f), or a last (else expr ...)
  (define (expand-cond env clauses)
    (if (null? clauses)
        undef
        (let ((test (caar clauses))
              (body (cdar clauses))
              (rest (cdr clauses)))
          (cond
           ((auxiliary? env test 'else)
            (unless (and (null? rest) (pair? body))
              (error 'expand-cond "invalid else clause" (car clauses)))
            `(,(marker 'begin) ,@body))
           ((null? body)
            (let ((t (gensym "cond")))
              `(,(marker 'let) ((,t ,test))
                (,(marker 'if) ,t ,t ,(expand-cond env rest)))))
           ((auxiliary? env (car body) '=>)
            (unless (= (length body) 2)
              (error 'expand-cond "invalid => clause" (car clauses)))
            (let ((t (gensym "cond")))
              `(,(marker 'let) ((,t ,test))
                (,(marker 'if) ,t (,(cadr body) ,t) ,(expand-cond env rest)))))
           (else
            `(,(marker 'if) ,test (,(marker 'begin) ,@body) ,(expand-cond env rest)))))))


  ;; A self-evaluating datum, or a literal
  (define (front-quoted x)
    (cond
     ((null? x) ''())
     ((or (immediate? x) (exact-rational? x) (string? x)) x)
     (else
      (let check ((y x))
        (cond
         ((pair? y) (check (car y)) (check (cdr y)))
         ((vector? y) (vector-for-each check y))
         ((not (or (symbol? y) (null? y) (boolean? y) (char? y) (exact-rational? y) (string? y)
                   (bytevector? y)))
          (error 'front-expr "unsupported quoted datum" y))))
      (make-literal x))))


  ;; `x as conses of its constant parts, quoted, and the values of its unquotes, a splice
  ;; appended; a nested quasiquote adds a level, which each unquote removes
  (define (expand-quasiquote env x)
    (define (form? y keyword)
      (and (list? y) (= (length y) 2) (auxiliary? env (car y) keyword)))
    (define (unquote? y)
      (or (form? y 'unquote) (form? y 'unquote-splicing)))
    (define (constant? y depth)
      (cond
       ((unquote? y) (and (> depth 0) (constant? (cadr y) (- depth 1))))
       ((form? y 'quasiquote) (constant? (cadr y) (+ depth 1)))
       ((pair? y) (and (constant? (car y) depth) (constant? (cdr y) depth)))
       ((vector? y) (constant? (vector->list y) depth))
       (else #t)))
    (define (keyword-form keyword y depth)
      `(,(core-ref 'cons) ,(make-literal keyword) (,(core-ref 'cons) ,(expand y depth) ''())))
    (define (expand y depth)
      (cond
       ((constant? y depth)
        (front-quoted y))
       ((and (unquote? y) (> depth 0))
        (keyword-form (car y) (cadr y) (- depth 1)))
       ((form? y 'unquote)
        (cadr y))
       ((form? y 'unquote-splicing)
        (error 'expand-quasiquote "unquote-splicing outside a list" y))
       ((form? y 'quasiquote)
        (keyword-form 'quasiquote (cadr y) (+ depth 1)))
       ((and (pair? y) (form? (car y) 'unquote-splicing) (= depth 0))
        `(,(core-ref 'append) ,(cadar y) ,(expand (cdr y) depth)))
       ((pair? y)
        `(,(core-ref 'cons) ,(expand (car y) depth) ,(expand (cdr y) depth)))
       (else
        `(,(core-ref 'list->vector) ,(expand (vector->list y) depth)))))
    (expand x 0))


  ;; Each clause is ((datum ...) expr ...), ((datum ...) => f), or a last (else expr ...) or
  ;; (else => f); the key is compared with eq? to immediates and symbols, with eqv? to bignums
  ;; and ratios
  (define (expand-case env expr)
    (let-values (((key clauses) (parse-case expr)))
      (let ((t (gensym "case")))
        (define (body clause)
          (if (auxiliary? env (cadr clause) '=>)
              (begin
                (unless (= (length clause) 3)
                  (error 'expand-case "invalid => clause" clause))
                `(,(caddr clause) ,t))
              `(,(marker 'begin) ,@(cdr clause))))
        (define (test x)
          (cond
           ((null? x) `(,(marker 'eq?) ,t ''()))
           ((symbol? x) `(,(marker 'eq?) ,t ,(make-literal x)))
           ((immediate? x) `(,(marker 'eq?) ,t ,x))
           ((exact-rational? x) `(,(core-ref 'eqv?) ,t ,x))
           (else (error 'expand-case "unsupported case datum" x))))
        `(,(marker 'let) ((,t ,key))
          ,(let loop ((clauses clauses))
             (cond
              ((null? clauses) undef)
              ((auxiliary? env (caar clauses) 'else)
               (unless (null? (cdr clauses))
                 (error 'expand-case "invalid else clause" (car clauses)))
               (body (car clauses)))
              (else
               (unless (list? (caar clauses))
                 (error 'expand-case "invalid case clause" (car clauses)))
               `(,(marker 'if)
                 (,(marker 'or) ,@(map test (caar clauses)))
                 ,(body (car clauses))
                 ,(loop (cdr clauses))))))))))


  ;; else and => are keywords unless bound
  (define (auxiliary? env x keyword)
    (and (eq? x keyword)
         (not (assq keyword env)))))
