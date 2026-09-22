;; compiler.ss

(import (chezscheme))


(define (acons k v alist)
  (cons (cons k v) alist))


;; x must be a non-empty list
(define (split-last x)
  (let loop ((x x)
             (acc '()))
    (if (null? (cdr x))
        (values (reverse acc) (car x))
        (loop (cdr x) (cons (car x) acc)))))


(define (read-all)
  (let loop ((acc '()))
    (let ((x (read)))
      (if (eof-object? x)
          (reverse acc)
          (loop (cons x acc))))))


(define (add acc . xs)
  (let loop ((acc acc)
             (xs xs))
    (if (null? xs)
        acc
        (loop (cons (car xs) acc)
              (cdr xs)))))

;; ----------------------------------------------------------------------------

(define (generator prefix)
  (let ((n 0))
    (lambda ()
      (set! n (+ n 1))
      (string-append prefix (number->string n)))))


(define gen-proc (generator "$proc"))


(define-record-type compile-expr-context
  (fields gen-var gen-label))


(define (new-expr-context)
  (make-compile-expr-context (generator "%tmp") (generator "@label")))


(define (fresh-var ctx)
  ((compile-expr-context-gen-var ctx)))


(define (fresh-label ctx)
  ((compile-expr-context-gen-label ctx)))


(define (encode-immediate? x)
  (cond
   ((integer? x)
    (+ (* x 8) 1))
   ((boolean? x)
    (if x 10 2))
   (else #f)))


(define *special-forms* '(if let))

;; compile expr
;;   env is an alist of pairs (symbol . variable)
;;   code is a reversed list of instructions
;; returns (values dst updated-code) or throw an error
(define (compile-expr ctx env expr code)
  (cond
   ((encode-immediate? expr)
    => (lambda (x)
         (let ((tmp (fresh-var ctx)))
           (values tmp
                   (cons (list 'copy tmp x) code)))))

   ((null? expr)
    (error 'compile-expr "invalid form" expr))

   ((and (list? expr)
         (not (null? expr))
         (symbol? (car expr))
         (memq (car expr) *special-forms*)
         (not (assq (car expr) env)))
    (case (car expr)
      ((if)
       (unless (= (length expr) 4)
         (error 'compile-expr "invalid arity for if"))
       (compile-if ctx env (list-ref expr 1) (list-ref expr 2) (list-ref expr 3) code))

      ((let)
       (unless (= (length expr) 3)
         (error 'compile-expr "invalid arity for let"))
       (compile-let ctx env (list-ref expr 1) (list-ref expr 2) code))

      (else
       (error 'compile-expr "unimplemented special form" expr))))

   ((and (list? expr)
         (not (null? expr)))
    (compile-call ctx env expr code))

   ((symbol? expr)
    (let ((x (assq expr env)))
      (if x
          (values (cdr x) code)
          (error 'compile-expr "unknown symbol" expr))))

   (else
    (error 'compile-expr "unknown form" expr))))


(define (compile-exprs ctx env exprs code)
  (let loop ((exprs exprs)
             (code code)
             (vars '()))
    (if (null? exprs)
        (values (reverse vars) code)
        (let-values (((x code) (compile-expr ctx env (car exprs) code)))
          (loop (cdr exprs) code (cons x vars))))))


(define (compile-if ctx env test-expr true-expr false-expr code)
  (let* ((label-true-start  (fresh-label ctx))
         (label-true-end    (fresh-label ctx))
         (label-false-start (fresh-label ctx))
         (label-false-end   (fresh-label ctx))
         (label-phi         (fresh-label ctx))
         (test-result       (fresh-var ctx))
         (dst               (fresh-var ctx)))
    (let*-values (((test-var code)
                   (compile-expr ctx env test-expr code))
                  ((true-var code)
                   (compile-expr ctx env true-expr
                                 (add code
                                      (list 'cnel test-result test-var (encode-immediate? #f))
                                      (list 'jnz test-result label-true-start label-false-start)
                                      (list 'label label-true-start))))
                  ((false-var code)
                   (compile-expr ctx env false-expr
                                 (add code
                                      (list 'label label-true-end)
                                      (list 'jmp label-phi)
                                      (list 'label label-false-start)))))
      (values dst
              (add code
                   (list 'label label-false-end)
                   (list 'jmp label-phi)
                   (list 'label label-phi)
                   (list 'phi dst label-true-end true-var label-false-end false-var))))))


(define (compile-let ctx env bindings expr code)
  (let loop ((bindings bindings)
             (new-env env)
             (code code))
    (if (null? bindings)
        (compile-expr ctx new-env expr code)
        (let ((k (caar bindings))
              (v (cadar bindings)))
          (let-values (((dst code) (compile-expr ctx env v code)))
            (loop (cdr bindings)
                  (acons k dst new-env)
                  code))))))


(define (compile-call ctx env form code)
  (let-values (((vars code) (compile-exprs ctx env form code)))
    (let* ((a (fresh-var ctx))
           (b (fresh-var ctx)))
      (values b
              (add code
                   (list 'call a "$check_procedure" (car vars) (- (length vars) 1))
                   (cons* 'call b a (cdr vars)))))))

;; ----------------------------------------------------------------------------

;; Convert a list of elements s.t.
;;   (define (foo x y) (+ x y)) ---> (foo "$proc42" (x y) (+ x y))
(define (scan-defs defs)
  (let loop ((acc '())
             (defs defs))
    (if (null? defs)
        (reverse acc)
        (let ((form (car defs)))
          (unless (and (list? form)
                       (not (null? form))
                       (eq? (car form) 'define))
            (error 'scan-defs "non define form in define position"))
          (let ((name (caadr form))
                (args (cdadr form))
                (body (caddr form)))
            (when (assq name acc)
              (error 'scan-defs "define: redefinition not allowed"))
            (loop (cons (list name (gen-proc) args body) acc)
                  (cdr defs)))))))


(define (compile-defs env defs top)
  (if (null? defs)
      top
      (let ((proc (list-ref (car defs) 1))
            (args (list-ref (car defs) 2))
            (body (list-ref (car defs) 3))
            (ctx (new-expr-context)))
        (let* ((vars (map (lambda (_) (fresh-var ctx)) args))
               (env-fn (append (map cons args vars) env)))
          (let-values (((ret code-fn) (compile-expr ctx env-fn body '())))
            (compile-defs env
                          (cdr defs)
                          (cons (cons* 'function proc vars
                                       (reverse (cons (list 'ret ret) code-fn)))
                                top)))))))


(define (compile-main env expr top)
  (let ((ctx (new-expr-context)))
    (let-values (((ret code) (compile-expr ctx env expr '())))
      (cons (cons 'main (reverse (cons (list 'ret ret) code)))
            top))))


;; TODO: what about a (define define 42) ?
(define (compile-program env program)
  (when (null? program)
    (error 'compile-program "empty program"))

  (let-values (((defs expr) (split-last program)))
    (let* ((defs (scan-defs defs))
           (env (append (map (lambda (x) (cons (car x) (cadr x))) defs) env))
           (top (compile-defs env defs '()))
           (top (compile-main env expr top)))
      (reverse top))))

;; ----------------------------------------------------------------------------

(define (emit-proc proc arity fn)
  (printf "data ~a = align 8 { l $procedure_type, l ~a, l ~a }\n" proc arity fn))

(define (emit expr)
  (define (p . x)
    (for-each display x)
    (newline))

  (define (pi . x)
    (display "    ")
    (apply p x))

  (unless (null? expr)
    (case (car expr)
      ((main)
       (let ((body (cdr expr)))
         (display "export function l $entry() {\n")
         (display "@start\n")
         (for-each emit body)
         (display "}\n\n")))

      ((function)
       (let ((name (cadr expr))
             (vars (caddr expr))
             (body (cdddr expr)))
         (emit-proc name (length vars) (string-append name "_fn"))
         (printf "function l ~a_fn(~{l ~a~^, ~}) {\n" name vars)
         (display "@start\n")
         (for-each emit body)
         (display "}\n\n")))

      ((copy)
       (pi (cadr expr) " =l copy " (caddr expr)))

      ((call)
       (printf "    ~a =l call ~a(~{l ~a~^, ~})\n" (cadr expr) (caddr expr) (cdddr expr)))

      ((ret)
       (pi "ret " (cadr expr)))

      ((label)
       (p (cadr expr)))

      ((jmp)
       (pi "jmp " (cadr expr)))

      ((cnel)
       (printf "    ~a =l cnel ~a, ~a\n" (list-ref expr 1) (list-ref expr 2) (list-ref expr 3)))

      ((jnz)
       (printf "    jnz ~a, ~a, ~a\n" (list-ref expr 1) (list-ref expr 2) (list-ref expr 3)))

      ((phi)
       (printf "    ~a =l phi ~a ~a, ~a ~a\n"
               (list-ref expr 1) (list-ref expr 2) (list-ref expr 3)
               (list-ref expr 4) (list-ref expr 5)))

      (else
       (error 'emit "unknown instruction " expr)))))

;; ----------------------------------------------------------------------------

(define *env* '())


(define (define-c-proc name c-name arity)
  (let ((proc (gen-proc)))
    (emit-proc proc arity (string-append "$" c-name))
    (set! *env* (acons name proc *env*))))


(define-c-proc 'eq? "eq" 2)
(define-c-proc '+   "int_add" 2)
(define-c-proc '-   "int_sub" 2)
(define-c-proc '*   "int_mul" 2)
(define-c-proc '=   "int_eq"  2)
(define-c-proc '<   "int_lt"  2)
(define-c-proc '<=  "int_leq" 2)
(define-c-proc '>   "int_gt"  2)
(define-c-proc '>=  "int_geq" 2)


(for-each emit (compile-program *env* (read-all)))
