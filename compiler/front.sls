;; The front pass: every local variable renamed to a fresh gensym, whose pretty name is the
;; original, and every special form headed by its marker, so a plain symbol at the head of a
;; form is a call

(library (front)
  (export front-def)
  (import (chezscheme)
          (utils)
          (parse))


  (define (fresh-names names)
    (map (lambda (x) (gensym (symbol->string x))) names))


  ;; A local's entry holds its fresh name, any other entry a binding record
  (define (bind env names fresh)
    (append (map cons names fresh) env))


  (define (rename-formals formals fresh)
    (cond
     ((null? formals) '())
     ((pair? formals) (cons (car fresh) (rename-formals (cdr formals) (cdr fresh))))
     (else (car fresh))))


  ;; (names . body): the names of a def's parameters, renamed, and its body, in env
  (define (front-def env names body)
    (let ((fresh (fresh-names names)))
      (cons fresh (front-body (bind env names fresh) body))))


  (define (front-body env body)
    (front-exprs env (expand-body env body)))


  (define (front-exprs env exprs)
    (map (lambda (x) (front-expr env x)) exprs))


  (define (front-expr env x)
    (cond
     ((symbol? x)
      (let ((entry (assq x env)))
        (if (and entry (symbol? (cdr entry))) (cdr entry) x)))
     ((or (immediate? x) (vector-literal? x) (not (non-empty-list? x)))
      x)
     (else
      (let ((keyword (special-keyword env x)))
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
           (let-values (((bindings body) (parse-let* x)))
             (let loop ((bindings bindings)
                        (env env)
                        (acc '()))
               (if (null? bindings)
                   `(,(marker 'let*) ,(reverse acc) ,@(front-body env body))
                   (let ((fresh (fresh-names (list (caar bindings)))))
                     (loop (cdr bindings)
                           (bind env (list (caar bindings)) fresh)
                           (cons (list (car fresh) (front-expr env (cadar bindings))) acc)))))))

          ((letrec letrec*)
           (let-values (((bindings body) (parse-letrec x)))
             (let* ((vars (map car bindings))
                    (fresh (fresh-names vars))
                    (inner (bind env vars fresh)))
               `(,(marker keyword) ,(map list fresh (front-exprs inner (map cadr bindings)))
                 ,@(front-body inner body)))))

          ;; else and => are walked as variables, renamed only when bound to a local
          ((cond)
           `(,(marker 'cond) ,@(map (lambda (c) (front-exprs env c)) (parse-cond x))))

          ((case)
           (let-values (((key clauses) (parse-case x)))
             `(,(marker 'case) ,(front-expr env key)
               ,@(map (lambda (c)
                        (cons (if (symbol? (car c)) (front-expr env (car c)) (car c))
                              (front-exprs env (cdr c))))
                      clauses))))

          ((define)
           (error 'front-expr "invalid context for definition" x))

          (else
           `(,(marker keyword) ,@(front-exprs env (cdr x))))))))))
