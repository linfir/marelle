;; Assignment conversion, after resolution: no local is assigned afterwards, and a closure body
;; binds its captures itself:
;; - an assigned and captured local to a box: (make-box v), box-ref, box-set!;
;; - an assigned local no lambda captures to a cell: (%cell v), (%cell-ref c), (%cell-set! c v),
;;   one slot of the function;
;; - a capture, inside its closure body, to a fresh local bound to (closure-ref (low raw i)).
;; An assigned local's binding gets a fresh name, its box or cell bound by a let around the
;; body; a set! is (begin store undef), its value bound to a fresh local first when it is a
;; closure, which the local names.

(library (assign)
  (export convert-assignments)
  (import (chezscheme)
          (parse)
          (vars)
          (closure)
          (defs))


  ;; core: (names . body)
  (define (convert-assignments core)
    (let-values (((names body) (convert-binder (car core) (cdr core) '())))
      (cons names body)))


  ;; renames: the captures of the enclosing closure body, to their local there
  (define (convert-body body renames)
    (map (lambda (x) (convert x renames)) body))


  (define (convert x renames)
    (cond
     ((symbol? x)
      (convert-read x renames))
     ((or (immediate? x) (not (pair? x)))
      x)
     (else
      (case (special-keyword x)
        ((closure)
         (convert-closure x renames))
        ((let)
         (if (named-let? x)
             (let-values (((name bindings body) (parse-named-let x)))
               (let-values (((vars body) (convert-binder (map car bindings) body renames)))
                 `(,(car x) ,name ,(convert-bindings vars bindings renames) ,@body)))
             (let-values (((bindings body) (parse-let x)))
               (let-values (((vars body) (convert-binder (map car bindings) body renames)))
                 `(,(car x) ,(convert-bindings vars bindings renames) ,@body)))))
        ((fix)
         `(,(car x) ,(convert-bindings (map car (cadr x)) (cadr x) renames)
           ,@(convert-body (cddr x) renames)))
        ((set!)
         (convert-set! x renames))
        ;; The name only names the variable in the error
        ((check-defined)
         `(,(car x) ,(cadr x) ,@(convert-body (cddr x) renames)))
        ((low)
         `(,(car x) ,(cadr x) ,@(convert-body (cddr x) renames)))
        ((const)
         x)
        ((#f)
         (convert-body x renames))
        (else
         (cons (car x) (convert-body (cdr x) renames)))))))


  (define (convert-bindings vars bindings renames)
    (map (lambda (var b) (list var (convert (cadr b) renames))) vars bindings))


  (define (renamed x renames)
    (cond
     ((assq x renames) => cdr)
     (else x)))


  (define (convert-read x renames)
    (let ((y (renamed x renames)))
      (cond
       ((boxed? x) `(,(core-def 'box-ref) ,y))
       ((assigned? x) `(,(marker '%cell-ref) ,y))
       (else y))))


  (define (convert-set! x renames)
    (let-values (((var value) (parse-set! x)))
      (let* ((y (renamed var renames))
             (value (convert value renames))
             (store (lambda (v)
                      (if (boxed? var)
                          `(,(core-def 'box-set!) ,y ,v)
                          `(,(marker '%cell-set!) ,y ,v)))))
        (if (eq? (special-keyword value) 'closure)
            (let ((local (fresh-local var)))
              `(,(marker 'let) ((,local ,value)) ,(store local) ,undef))
            `(,(marker 'begin) ,(store value) ,undef)))))


  ;; The names bound around body, each assigned one renamed to a fresh local of its initial
  ;; value, and body converted, in a let binding the assigned ones to their box or cell
  (define (convert-binder names body renames)
    (let ((news (map (lambda (x) (if (assigned? x) (fresh-local x) x)) names))
          (body (convert-body body renames)))
      (values news
              (let ((cells (filter (lambda (b) (not (eq? (car b) (cdr b)))) (map cons names news))))
                (if (null? cells)
                    body
                    `((,(marker 'let)
                       ,(map (lambda (b)
                               (list (car b) (if (boxed? (car b))
                                                 `(,(core-def 'make-box) ,(cdr b))
                                                 `(,(marker '%cell) ,(cdr b)))))
                             cells)
                       ,@body)))))))


  ;; The body's captures are renamed to fresh locals, bound by closure-ref before the
  ;; parameters' boxes and cells
  (define (convert-closure x renames)
    (let-values (((captures formals body) (parse-closure x)))
      (let ((inner (map (lambda (c) (cons c (fresh-local c))) captures)))
        (let-values (((names body) (convert-binder (formals-names formals) body inner)))
          `(,(car x) ,(map (lambda (c) (renamed c renames)) captures)
            ,(rename-formals formals names)
            ,@(if (null? inner)
                  body
                  `((,(marker 'let)
                     ,(map (lambda (c i)
                             (list (cdr c) `(,(core-def 'closure-ref) (,(marker 'low) raw ,i))))
                           inner (iota (length inner)))
                     ,@body))))))))


  (define (rename-formals formals names)
    (cond
     ((null? formals) '())
     ((pair? formals) (cons (car names) (rename-formals (cdr formals) (cdr names))))
     (else (car names)))))
