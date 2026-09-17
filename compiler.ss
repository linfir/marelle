;; compiler.ss

(import (chezscheme))


(define (acons k v alist)
  (cons (cons k v) alist))


;; almost like map, but with left-to-right evaluation order
(define (my-map f lst)
  (let loop ((acc '())
             (lst lst))
    (if (null? lst)
        (reverse acc)
        (loop (cons (f (car lst)) acc) (cdr lst)))))


;; x must be a non-empty list
(define (split-last x)
  (let loop ((x x) (acc '()))
    (if (null? (cdr x))
        (cons (reverse acc) (car x))
        (loop (cdr x) (cons (car x) acc)))))


(define (read-all)
  (let loop ((acc '()))
    (let ((x (read)))
      (if (eof-object? x)
          (reverse acc)
          (loop (cons x acc))))))

;; ----------------------------------------------------------------------------

(define (generator prefix)
  (let ((n 0))
    (lambda ()
      (set! n (+ n 1))
      (string-append prefix (number->string n)))))


(define gen-var   (generator "%tmp"))
(define gen-label (generator "@label"))
(define gen-proc  (generator "$proc"))

(define (seq . xs)
  (cons 'seq
        (apply append
               (map (lambda (x)
                      (if (and (pair? x) (eq? (car x) 'seq))
                          (cdr x)
                          (list x)))
                    xs))))


(define (with-code instructions compiled)
  (cons (car compiled)
        (seq (apply seq instructions)
             (cdr compiled))))


(define (encode-immediate? x)
  (cond
   ((integer? x)
    (+ (* x 8) 1))
   ((boolean? x)
    (if x 10 2))
   (else #f)))


(define *special-forms* '(if let))


(define (compile-expr env expr)
  (cond
   ((encode-immediate? expr)
    => (lambda (x)
         (let ((tmp (gen-var)))
           (cons tmp (list 'copy tmp x)))))

   ((null? expr)
    (error 'compile-expr "invalid form" expr))

   ((and (list? expr)
         (not (null? expr))
         (symbol? (car expr))
         (memq (car expr) *special-forms*)
         (not (assq (car expr) env)))
    (case (car expr)
      ((if)
       (if (= (length expr) 4)
           (apply compile-if (my-map (lambda (e) (compile-expr env e)) (cdr expr)))
           (error 'compile-expr "invalid arity for if")))

      ((let)
       (if (= (length expr) 3)
           (compile-let env (cadr expr) (caddr expr))
           (error 'compile-expr "invalid arity for let")))

      (else
       (error 'compile-expr "unimplemented special form" expr))))

   ((and (list? expr)
         (not (null? expr)))
    (let* ((pairs (my-map (lambda (e) (compile-expr env e)) expr))
           (vars (map car pairs))
           (codes (map cdr pairs)))
      (with-code codes (compile-call (car vars) (cdr vars)))))

   ((symbol? expr)
    (let ((x (assq expr env)))
      (if x
          (cons (cdr x) (seq))
          (error 'compile-expr "unknown symbol" expr))))

   (else
    (error 'compile-expr "unknown form" expr))))


(define (compile-call f args)
  (let* ((a (gen-var))
         (b (gen-var)))
    (cons b
          (seq (list 'call a "$check_procedure" f (length args))
               (cons* 'call b a args)))))


(define (compile-if test true false)
  (let* ((label-true-start  (gen-label))
         (label-true-end    (gen-label))
         (label-false-start (gen-label))
         (label-false-end   (gen-label))
         (label-phi         (gen-label))
         (dst (gen-var)))
    (cons dst
          (seq
           (cdr test)
           (list 'test (car test) label-true-start label-false-start)

           (list 'label label-true-start)
           (cdr true)
           (list 'label label-true-end)
           (list 'jmp label-phi)

           (list 'label label-false-start)
           (cdr false)
           (list 'label label-false-end)
           (list 'jmp label-phi)

           (list 'label label-phi)
           (list 'phi dst label-true-end (car true) label-false-end (car false))))))


(define (compile-let env bindings expr)
  (let loop ((bindings bindings)
             (new-env env)
             (code-lst '()))
    (if (null? bindings)
        (with-code code-lst (compile-expr new-env expr))
        (let ((k (caar bindings))
              (v (cadar bindings)))
          (let ((c (compile-expr env v)))
            (loop (cdr bindings)
                  (acons k (car c) new-env)
                  (cons (cdr c) code-lst)))))))

;; ----------------------------------------------------------------------------

;; Convert a list of elements s.t.
;;   (define (foo x y) (+ x y)) ---> (foo "$proc42" (x y) (+ x y))
(define (scan-defines defs)
  (let loop ((acc '())
             (defs defs))
    (if (null? defs)
        (reverse acc)
        (let ((form (car defs)))
          (unless (and (list? form)
                       (not (null? form))
                       (eq? (car form) 'define))
            (error 'scan-defines "non define form in define position"))
          (let ((name (caadr form))
                (args (cdadr form))
                (body (caddr form)))
            (when (assq name acc)
              (error 'scan-defines "define: redefinition not allowed"))
            (loop (cons (list name (gen-proc) args body) acc)
                  (cdr defs)))))))


;; What about a (define define 42) ?
(define (compile-program env program)
  (when (null? program)
    (error 'compile-program "empty program"))

  (let* ((split (split-last program))
         (defs  (scan-defines (car split)))
         (expr  (cdr split))
         (env2  (append (map (lambda (x) (cons (car x) (cadr x))) defs)
                        env)))
    (seq
     (cons 'seq
           (map (lambda (def)
                  (let ((name (list-ref def 0))
                        (proc (list-ref def 1))
                        (args (list-ref def 2))
                        (body (list-ref def 3)))
                    (let* ((vars (my-map (lambda (_) (gen-var)) args))
                           (env3 (append (map cons args vars) env2))
                           (c (compile-expr env3 body)))
                      (list 'function proc vars
                            (seq (cdr c)
                                 (list 'ret (car c)))))))
                defs))
     (list 'main
           (let ((c (compile-expr env2 expr)))
             (seq (cdr c) (list 'ret (car c))))))))

;; ----------------------------------------------------------------------------

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
       (let ((body (cadr expr)))
         (printf "export function l $entry() {\n")
         (emit (list 'label "@start"))
         (emit body)
         (display "}\n\n")))

      ((function)
       (let ((name (list-ref expr 1))
             (vars (list-ref expr 2))
             (body (list-ref expr 3)))
         (printf "function l ~a_fn(~{l ~a~^, ~}) {\n" name vars)
         (emit (list 'label "@start"))
         (emit body)
         (display "}\n")
         (emit-proc name (length vars) (string-append name "_fn"))))

      ((copy)
       (pi (cadr expr) " =l copy " (caddr expr)))

      ((call)
       (printf "    ~a =l call ~a(~{l ~a~^, ~})\n"
               (cadr expr) (caddr expr) (cdddr expr)))

      ((seq)
       (for-each emit (cdr expr)))

      ((ret)
       (pi "ret " (cadr expr)))

      ((label)
       (p (cadr expr)))

      ((jmp)
       (pi "jmp " (cadr expr)))

      ((test)
       (let ((v (gen-var)))
         (pi v " =l cnel " (cadr expr) ", 2")
         (pi "jnz " v ", " (caddr expr) ", " (cadddr expr))))

      ((phi)
       (pi (list-ref expr 1) " =l phi "
           (list-ref expr 2) " " (list-ref expr 3) ", "
           (list-ref expr 4) " " (list-ref expr 5)))

      (else
       (error 'emit "unknown instruction " expr)))))


(define (emit-proc proc arity fn)
  (printf "data ~a = align 8 { l $procedure_type, l ~a, l ~a }\n" proc arity fn))

                                        ; ----------------------------------------------------------------------------

(define *env* '())

(define (define-c-proc name c-name arity)
  (let ((proc (gen-proc)))
    (printf "data ~a = align 8 { l $procedure_type, l ~a, l $~a }\n" proc arity c-name)
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



(emit (compile-program *env* (read-all)))
