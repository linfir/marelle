;; Name resolution, after closure conversion: every name but the locals to an explicit form,
;; so a later pass needs no env of bindings:
;; - a def or a data name to its record, printed as its name, a constant to (const operand);
;; - a call of a low operation to (low op arg ...);
;; - a top-level variable of main to calls of global-ref and global-set!, a let binding it
;;   getting a fresh local stored in its cell;
;; - (check-defined x [value]) to (check-defined x read [value]).
;; The calls of the library's variadic defs with a number of operands their inline def does not
;; take are expanded in terms of it.
;; Locals, gensyms since the front pass, stay symbols.

(library (resolve)
  (export resolve-body)
  (import (chezscheme)
          (utils)
          (parse)
          (vars)
          (closure)
          (defs))


  (define (resolve-body env body)
    (map (lambda (x) (resolve env x)) body))


  (define (resolve env x)
    (cond
     ((symbol? x)
      (resolve-variable env x))
     ((or (immediate? x) (not (pair? x)))
      x)
     (else
      (case (special-keyword x)
        ((closure)
         (let-values (((captures formals body) (parse-closure x)))
           `(,(car x) ,captures ,formals ,@(resolve-body env body))))
        ((let)
         (resolve-let env x))
        ((fix)
         `(,(car x) ,(map (lambda (b) (list (car b) (resolve env (cadr b)))) (cadr x))
           ,@(resolve-body env (cddr x))))
        ((set!)
         (resolve-set! env x))
        ((check-defined)
         `(,(car x) ,(cadr x) ,(resolve env (cadr x)) ,@(resolve-body env (cddr x))))
        ((core)
         (core-def (cadr x)))
        ((#f)
         (resolve-call env x))
        (else
         (cons (car x) (resolve-body env (cdr x))))))))


  (define (binding env x)
    (let ((entry (assq x env)))
      (and entry (cdr entry))))


  (define (global-ref i)
    `(,(core-def 'global-ref) (,(marker 'low) raw ,i)))


  (define (global-set i x)
    `(,(core-def 'global-set!) (,(marker 'low) raw ,i) ,x))


  (define (resolve-variable env x)
    (cond
     ((top-global-index x) => global-ref)
     ((gensym? x) x)
     (else
      (let ((b (binding env x)))
        (cond
         ((def-binding? b) (def-binding-def b))
         ((data-binding? b) (data-binding-data b))
         ((constant-binding? b) `(,(marker 'const) ,(constant-binding-operand b)))
         ((low-op-binding? b) (compile-error 'compile-value "low operation used as a value" x))
         (else (compile-error 'lookup "unknown symbol" x)))))))


  (define (resolve-let env x)
    (if (named-let? x)
        (let-values (((name bindings body) (parse-named-let x)))
          `(,(car x) ,name ,(map (lambda (b) (list (car b) (resolve env (cadr b)))) bindings)
            ,@(resolve-body env body)))
        (let-values (((bindings body) (parse-let x)))
          (let ((locals (map (lambda (b)
                               (if (top-global-index (car b)) (fresh-local (car b)) (car b)))
                             bindings)))
            `(,(car x) ,(map (lambda (local b) (list local (resolve env (cadr b)))) locals bindings)
              ,@(apply append
                       (map (lambda (local b)
                              (let ((i (top-global-index (car b))))
                                (if i (list (global-set i local)) '())))
                            locals bindings))
              ,@(resolve-body env body))))))


  (define (resolve-set! env x)
    (let-values (((var value) (parse-set! x)))
      (cond
       ((top-global-index var)
        => (lambda (i)
             (let ((local (fresh-local var)))
               `(,(marker 'let) ((,local ,(resolve env value)))
                 ,(global-set i local)
                 ,undef))))
       ((gensym? var)
        `(,(car x) ,var ,(resolve env value)))
       ((binding env var)
        (compile-error 'compile-set! "set! of a library procedure or primitive" var))
       (else
        (compile-error 'lookup "unknown symbol" var)))))


  (define (resolve-call env x)
    (let ((b (and (symbol? (car x)) (not (gensym? (car x))) (binding env (car x)))))
      (if (low-op-binding? b)
          `(,(marker 'low) ,(low-op-binding-name b) ,@(resolve-body env (cdr x)))
          (let ((head (resolve env (car x))))
            (cond
             ((and (def? head) (expand-variadic head (cdr x)))
              => (lambda (expanded) (resolve env expanded)))
             (else
              (cons head (resolve-body env (cdr x)))))))))


  ;; The calls of the library's +, *, -, comparisons, bitwise operations, make-vector and list
  ;; whose number of operands the inline def does not take, in terms of it, or #f; the others
  ;; go to their general def
  (define (expand-variadic op args)
    (let ((def op))
      (and (general-def def)
           (let ()
             (define (fold-operands)
               (fold-left (lambda (acc x) (list op acc x)) (car args) (cdr args)))
             (case (def-name def)
               ((+ * bitwise-and bitwise-ior bitwise-xor)
                (let ((unit (case (def-name def)
                              ((*) 1)
                              ((bitwise-and) -1)
                              (else 0))))
                  (case (length args)
                    ((0) unit)
                    ((1) (list op unit (car args)))
                    ((2) #f)
                    (else (fold-operands)))))
               ((-)
                (case (length args)
                  ((1) (list op 0 (car args)))
                  ((0 2) #f)
                  (else (fold-operands))))
               ;; Each operand once, compared with the next
               ((= < > <= >=)
                (and (> (length args) 2)
                     (let ((tmps (map (lambda (x) (gensym "operand")) args)))
                       `(,(marker 'let) ,(map list tmps args)
                         ,(expand-and (map (lambda (x y) (list op x y))
                                           (list-head tmps (- (length tmps) 1))
                                           (cdr tmps)))))))
               ((make-vector)
                (and (= (length args) 1) (list op (car args) 0)))
               ;; Nested conses, so no frame and no rest list
               ((list)
                (case (length args)
                  ((0) ''())
                  ((1) #f)
                  (else (list (core-def 'cons) (car args) (cons op (cdr args))))))
               (else #f)))))))
