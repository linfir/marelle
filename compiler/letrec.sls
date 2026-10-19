;; The letrec pass: each letrec* of the front pass's output to let, fix and set!, after
;; Waddell, Sarkar and Dybvig, "Fixing Letrec", and Ghuloum and Dybvig, "Fixing Letrec
;; (reloaded)". In order, each binding is
;; - lambda: an unassigned variable bound to a lambda, in one fix around the rest;
;; - simple: a constant, an unassigned local or a call to an effect-free primitive on simple
;;   operands, bound in a let, when nothing evaluated before it may read it; a call may fail,
;;   so only before every complex init;
;; - complex: a cell, starting unbound, assigned its init in order; a read that may come before
;;   the init, or a set! before it, is checked (check-defined);
;; - unreferenced: dropped, or its init evaluated for effect when complex.
;; A named let whose name is used other than called in exit position, outside lambdas, is
;; ((letrec* ((name (lambda vars body ...))) name) init ...).
;; Names are unique, so a variable is known by its symbol alone.

(library (letrec)
  (export fix-letrec)
  (import (chezscheme)
          (utils)
          (parse))


  ;; Per def: the number of references of each local, a set! counting, and the assigned ones
  (define refs (make-parameter #f))
  (define sets (make-parameter #f))

  ;; Per def: true of the global names of the effect-free primitives
  (define pure? (make-parameter #f))


  ;; The body of a def, with its letrec* forms fixed
  (define (fix-letrec body pure)
    (parameterize ((refs (make-eq-hashtable))
                   (sets (make-eq-hashtable))
                   (pure? pure))
      (count-exprs! body)
      (map fix-expr body)))


  (define (count-exprs! exprs)
    (for-each count-expr! exprs))


  (define (count-expr! x)
    (cond
     ((symbol? x)
      (hashtable-update! (refs) x (lambda (n) (+ n 1)) 0))
     ((or (immediate? x) (not (pair? x))))
     (else
      (case (special-keyword x)
        ((lambda)
         (count-exprs! (cddr x)))
        ((let)
         (let-values (((name bindings body) (parse-any-let x)))
           (count-exprs! (map cadr bindings))
           (count-exprs! body)))
        ((letrec*)
         (count-exprs! (map cadr (cadr x)))
         (count-exprs! (cddr x)))
        ((set!)
         (let-values (((var value) (parse-set! x)))
           (hashtable-set! (sets) var #t)
           (count-exprs! (list var value))))
        ((#f)
         (count-exprs! x))
        (else
         (count-exprs! (cdr x)))))))


  ;; A let or named let => (values name-or-#f bindings body)
  (define (parse-any-let x)
    (if (named-let? x)
        (parse-named-let x)
        (let-values (((bindings body) (parse-let x)))
          (values #f bindings body))))


  (define (fix-expr x)
    (cond
     ((or (symbol? x) (immediate? x) (not (pair? x)))
      x)
     (else
      (case (special-keyword x)
        ((lambda)
         `(,(car x) ,(cadr x) ,@(map fix-expr (cddr x))))
        ((let)
         (let-values (((name bindings body) (parse-any-let x)))
           (if (and name (not (loop-body? name body)))
               (fix-expr `(,(marker 'letrec*) ((,name (,(marker 'lambda) ,(map car bindings) ,@body)))
                           (,name ,@(map cadr bindings))))
               `(,(car x) ,@(if name (list name) '())
                 ,(map (lambda (b) (list (car b) (fix-expr (cadr b)))) bindings)
                 ,@(map fix-expr body)))))
        ((letrec*)
         (fix-letrec* (map car (cadr x)) (map (lambda (b) (fix-expr (cadr b))) (cadr x))
                      (map fix-expr (cddr x))))
        ((set!)
         (let-values (((var value) (parse-set! x)))
           `(,(car x) ,var ,(fix-expr value))))
        ((#f)
         (map fix-expr x))
        (else
         (cons (car x) (map fix-expr (cdr x))))))))


  ;; The named let name is only called in exit position of body, as compile-exit compiles it
  ;; to a jump: through if, let, named let, letrec*, fix, begin, and the last operand of and
  ;; and or
  (define (loop-body? name body)
    (let-values (((init last) (split-last body)))
      (and (not (mentioned? name init))
           (loop-exit? name last))))


  (define (loop-exit? name x)
    (cond
     ((or (not (pair? x)) (immediate? x))
      (not (eq? x name)))
     (else
      (case (special-keyword x)
        ((if)
         (and (not (mentioned? name (cadr x)))
              (for-all (lambda (y) (loop-exit? name y)) (cddr x))))
        ((let)
         (let-values (((n bindings body) (parse-any-let x)))
           (and (not (mentioned? name (map cadr bindings)))
                (loop-body? name body))))
        ((letrec* fix)
         (and (not (mentioned? name (map cadr (cadr x))))
              (loop-body? name (cddr x))))
        ((begin and or)
         (or (null? (cdr x)) (loop-body? name (cdr x))))
        ((#f)
         (not (mentioned? name (if (eq? (car x) name) (cdr x) x))))
        (else
         (not (mentioned? name x)))))))


  (define (mentioned? x y)
    (pair? (mentions y (list x))))


  (define (assigned? x)
    (hashtable-ref (sets) x #f))


  (define (referenced? x)
    (> (hashtable-ref (refs) x 0) 0))


  (define (lambda-form? x)
    (eq? (special-keyword x) 'lambda))


  ;; Locals are the gensyms of the front pass; a global may change in any call
  (define (simple-init? x simple-vars group)
    (cond
     ((or (immediate? x) (exact-rational? x) (string? x) (literal? x))
      #t)
     ((symbol? x)
      (and (gensym? x)
           (not (assigned? x))
           (or (not (memq x group)) (memq x simple-vars))))
     (else
      (and (list? x)
           (or (and (symbol? (car x)) (not (gensym? (car x))))
               (eq? (special-keyword (car x)) 'core))
           ((pure?) (car x))
           (for-all (lambda (y) (simple-init? y simple-vars group)) (cdr x))))))


  (define (call? x)
    (and (pair? x) (not (immediate? x))))


  ;; The variables of vars that x mentions, a set! target included
  (define (mentions x vars)
    (let loop ((x x) (acc '()))
      (cond
       ((symbol? x) (if (and (memq x vars) (not (memq x acc))) (cons x acc) acc))
       ((or (immediate? x) (not (pair? x))) acc)
       (else (loop (cdr x) (loop (car x) acc))))))


  ;; x with each read and set! of a variable of vars checked
  (define (check-reads x vars)
    (cond
     ((symbol? x)
      (if (memq x vars) `(,(marker 'check-defined) ,x) x))
     ((or (immediate? x) (not (pair? x)))
      x)
     ;; (check-defined x value): the value, then the check
     ((eq? (special-keyword x) 'set!)
      (let ((value (check-reads (caddr x) vars)))
        `(,(car x) ,(cadr x) ,(if (memq (cadr x) vars)
                                  `(,(marker 'check-defined) ,(cadr x) ,value)
                                  value))))
     (else
      (cons (check-reads (car x) vars) (check-reads (cdr x) vars)))))


  (define (fix-letrec* vars inits body)
    (let ((lambda-vars (filter (lambda (x) x)
                               (map (lambda (x init)
                                      (and (lambda-form? init) (not (assigned? x)) x))
                                    vars inits))))
      ;; observed: the variables of the group the complex inits so far mention
      (let loop ((xs vars) (inits inits)
                 (simple '()) (lambdas '()) (complex '()) (seq '()) (observed '()))
        (if (null? xs)
            (finish-letrec* (reverse simple) (reverse lambdas) (reverse complex) (reverse seq)
                            body)
            (let ((x (car xs))
                  (init (car inits)))
              (cond
               ((memq x lambda-vars)
                (loop (cdr xs) (cdr inits) simple
                      (if (referenced? x) (cons (list x init) lambdas) lambdas)
                      complex seq observed))
               ;; An unreferenced call is kept for effect
               ((and (simple-init? init (map car simple) vars)
                     (not (memq x observed))
                     (not (exists (lambda (v) (memq v observed)) lambda-vars))
                     (or (not (call? init)) (and (null? seq) (referenced? x))))
                (loop (cdr xs) (cdr inits)
                      (if (referenced? x) (cons (list x init) simple) simple)
                      lambdas complex seq observed))
               (else
                (let ((observed (append (mentions init vars) observed)))
                  (if (referenced? x)
                      (loop (cdr xs) (cdr inits) simple lambdas (cons x complex)
                            (cons (cons x init) seq) observed)
                      (loop (cdr xs) (cdr inits) simple lambdas complex
                            (cons (cons #f init) seq) observed))))))))))


  ;; seq: (var . init) for each complex binding in order, var #f when unreferenced;
  ;; the lambdas check every complex variable, a complex init those from its own on
  (define (finish-letrec* simple lambdas complex seq body)
    (let* ((sets (let loop ((seq seq) (later complex))
                   (if (null? seq)
                       '()
                       (let* ((x (caar seq))
                              (init (check-reads (cdar seq) later))
                              (later (if x (cdr later) later)))
                         (cons (if x `(,(marker 'set!) ,x ,init) init)
                               (loop (cdr seq) later))))))
           (body (append sets body))
           (body (if (null? lambdas)
                     body
                     `((,(marker 'fix)
                        ,(map (lambda (b) (list (car b) (check-reads (cadr b) complex))) lambdas)
                        ,@body))))
           (body (if (null? complex)
                     body
                     `((,(marker 'let) ,(map (lambda (x) (list x unbound)) complex) ,@body))))
           (body (fold-right (lambda (b body) `((,(marker 'let) (,b) ,@body))) body simple)))
      (if (null? (cdr body))
          (car body)
          `(,(marker 'begin) ,@body)))))
