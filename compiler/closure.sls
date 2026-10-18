;; Closure conversion: each lambda to (closure (capture ...) formals body ...), its captures
;; the free locals it keeps in its closure, in first-reference order.
;; A lambda of a fix group is static when it captures nothing but the group's static ones: its
;; name is then a static procedure, captured by no lambda, and so are the names of named lets.
;; Runs after the analysis, whose free locals it reads; names are unique.

(library (closure)
  (export convert-closures parse-closure)
  (import (chezscheme)
          (parse)
          (vars))


  ;; Per def: the static names
  (define statics (make-parameter #f))


  (define (convert-closures body)
    (parameterize ((statics (make-eq-hashtable)))
      (map convert body)))


  ;; (closure captures formals body ...) => (values captures formals body)
  (define (parse-closure x)
    (values (cadr x) (caddr x) (cdddr x)))


  (define (static! names)
    (for-each (lambda (x) (hashtable-set! (statics) x #t)) names))


  (define (captures form)
    (filter (lambda (x) (not (hashtable-ref (statics) x #f))) (free-locals form)))


  (define (convert x)
    (cond
     ((or (symbol? x) (immediate? x) (vector-literal? x) (not (pair? x)))
      x)
     (else
      (case (special-keyword x)
        ((lambda)
         (convert-lambda x (captures x)))

        ((let)
         (if (named-let? x)
             (let-values (((name bindings body) (parse-named-let x)))
               (static! (list name))
               `(,(car x) ,name ,(convert-bindings bindings) ,@(map convert body)))
             (let-values (((bindings body) (parse-let x)))
               `(,(car x) ,(convert-bindings bindings) ,@(map convert body)))))

        ;; All static at first, then each capturing something is not, until none is left
        ((fix)
         (let ((names (map car (cadr x)))
               (forms (map cadr (cadr x))))
           (static! names)
           (let loop ()
             (let ((b (find (lambda (b)
                              (and (hashtable-ref (statics) (car b) #f) (pair? (captures (cadr b)))))
                            (cadr x))))
               (when b
                 (hashtable-delete! (statics) (car b))
                 (loop))))
           `(,(car x) ,(map (lambda (name form) (list name (convert-lambda form (captures form))))
                            names forms)
             ,@(map convert (cddr x)))))

        ((#f)
         (map convert x))

        (else
         (cons (car x) (map convert (cdr x))))))))


  (define (convert-bindings bindings)
    (map (lambda (b) (list (car b) (convert (cadr b)))) bindings))


  (define (convert-lambda x captures)
    (let-values (((formals body) (parse-lambda x)))
      `(,(marker 'closure) ,captures ,formals ,@(map convert body)))))
