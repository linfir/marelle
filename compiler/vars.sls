(library (vars)
  (export make-local-binding local-binding? local-binding-operand
          make-def-binding def-binding? def-binding-def
          make-data-binding data-binding? data-binding-data
          make-low-op-binding low-op-binding? low-op-binding-name
          make-constant-binding constant-binding? constant-binding-operand
          make-loop-binding loop-binding? loop-binding-label loop-binding-slots loop-binding-state
          var-facts lambda-frees top-globals top-global-index
          analyze-def! free-locals boxed? assigned?)
  (import (chezscheme)
          (parse))


  (define-record-type local-binding
    (fields operand))


  (define-record-type def-binding
    (fields def))


  (define-record-type data-binding
    (fields data))


  ;; In an unsafe define: a low operation such as raw+ or load, only called
  (define-record-type low-op-binding
    (fields name))


  ;; In an unsafe define: undef
  (define-record-type constant-binding
    (fields operand))


  ;; The name of a named let, only called in exit position: state is here, outer (a named let
  ;; used as a value inside it) or lambda (a lambda inside it)
  (define-record-type loop-binding
    (fields label slots state))


  ;; Per compilation: each local's facts, and each lambda's free locals in the order they are
  ;; first referenced, a set! counting after its value
  (define var-facts (make-parameter #f))
  (define lambda-frees (make-parameter #f))

  ;; Per compilation: main's top-level variables held in global cells, to their index
  (define top-globals (make-parameter #f))


  (define (top-global-index x)
    (hashtable-ref (top-globals) x #f))


  ;; owner: the lambda form binding the local, or the def's token
  (define-record-type facts
    (fields owner (mutable assigned?) (mutable captured?)))


  ;; Fills the tables from a def's core parameters' names and body
  (define (analyze-def! names body)
    (let ((owners (list (list 'def))))
      (bind! names (car owners))
      (analyze-exprs! body owners)))


  (define (bind! names owner)
    (for-each (lambda (x) (hashtable-set! (var-facts) x (make-facts owner #f #f))) names))


  ;; owners: the enclosing lambda forms, innermost first, then the def's token
  (define (analyze-exprs! exprs owners)
    (for-each (lambda (x) (analyze-expr! x owners)) exprs))


  (define (analyze-expr! x owners)
    (cond
     ((symbol? x)
      (reference! x owners))

     ((or (immediate? x) (not (pair? x))))

     (else
      (case (special-keyword x)
        ((lambda)
         (let-values (((formals body) (parse-lambda x)))
           (bind! (formals-names formals) x)
           (hashtable-set! (lambda-frees) x '())
           (analyze-exprs! body (cons x owners))
           (hashtable-update! (lambda-frees) x reverse #f)))

        ((let)
         (let-values (((name bindings body) (if (named-let? x)
                                                (parse-named-let x)
                                                (let-values (((bindings body) (parse-let x)))
                                                  (values #f bindings body)))))
           (analyze-exprs! (map cadr bindings) owners)
           (bind! (if name (cons name (map car bindings)) (map car bindings)) (car owners))
           (analyze-exprs! body owners)))

        ;; The names are bound in the lambdas too
        ((fix)
         (bind! (map car (cadr x)) (car owners))
         (analyze-exprs! (map cadr (cadr x)) owners)
         (analyze-exprs! (cddr x) owners))

        ((set!)
         (let-values (((var value) (parse-set! x)))
           (analyze-expr! value owners)
           (let ((f (reference! var owners)))
             (when f
               (facts-assigned?-set! f #t)))))

        ((#f)
         (analyze-exprs! x owners))

        (else
         (analyze-exprs! (cdr x) owners))))))


  ;; A reference to x from the innermost owner: x is free in each lambda up to its own;
  ;; the facts of x, or #f when it is not a local
  (define (reference! x owners)
    (let ((f (hashtable-ref (var-facts) x #f)))
      (when f
        (let loop ((owners owners))
          (unless (eq? (car owners) (facts-owner f))
            (facts-captured?-set! f #t)
            (hashtable-update! (lambda-frees) (car owners)
                               (lambda (frees) (if (memq x frees) frees (cons x frees)))
                               '())
            (loop (cdr owners)))))
      f))


  ;; The free locals of a lambda form
  (define (free-locals form)
    (or (hashtable-ref (lambda-frees) form #f)
        (error 'free-locals "lambda not analyzed" form)))


  (define (fact var accessor)
    (let ((f (hashtable-ref (var-facts) var #f)))
      (and f (accessor f))))


  (define (assigned? var)
    (fact var facts-assigned?))


  (define (boxed? var)
    (and (fact var facts-assigned?) (fact var facts-captured?))))
