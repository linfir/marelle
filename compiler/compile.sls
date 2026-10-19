;; Functions: each procedure, low def and literal init to its blocks and entries, and the
;; helpers emitting calls, frames and results; expressions go through the compilation's
;; expression compiler (expr.sls).
;;
;; There are 4 kinds:
;; - raw    a plain word
;; - val    a tagged value
;; - never  does not return
;; - void   returns nothing

(library (compile)
  (export compile-procedure compile-low-def compile-literal-init
          make-expressions current-expressions current-function kind check-kind literal-ref
          def-called? call-def direct-frame frame-store! emit-call! frame-call call-def-tail
          finish-result disable-loops fill-captures)
  (import (chezscheme)
          (utils)
          (parse)
          (vars)
          (defs)
          (gc-check)
          (output)
          (emit)
          (copy-prop))


  ;; The expression compiler of the compilation, expr.sls's (scheme-expressions):
  ;; - (inline def ops form): the value of the inline def on the operands ops
  ;; - (inline-branch def ops form jump-if label): a jump to label when the inline def's value
  ;;   on ops has the truth jump-if
  ;; - (body env def exprs): the body exprs, ending the function of def
  (define-record-type expressions
    (fields inline inline-branch body))


  (define current-expressions (make-parameter #f))


  ;; The def of the function being compiled, for self tail calls
  (define current-function (make-parameter #f))


  (define (kind x)
    (cond
     ((string? x) 'raw)
     ((memq (car x) '(quad as-raw)) 'raw)
     ((memq (car x) '(raw never void)) (car x))
     (else 'val)))


  ;; k is a kind, or any for a val or a raw
  (define (check-kind x k expr)
    (unless (if (eq? k 'any) (memq (kind x) '(val raw)) (eq? (kind x) k))
      (error 'check-kind (if (eq? k 'any) "expected a val or a raw" (format "expected a ~a" k))
             expr)))


  ;; The load of the literal cell of the string or symbol x
  (define (literal-ref x)
    (emit-op! 'load 'val (list (literals-symbol) `(quad ,(intern-literal! x)))))


  ;; A def reached by a Scheme call, with a frame, rather than inlined or a noalloc call
  (define (def-called? def)
    (not (or (def-inline? def) (def-noalloc? def))))


  ;; Calls def with the operands ops
  (define (call-def def ops form)
    (cond
     ((def-inline? def)
      ((expressions-inline (current-expressions)) def ops form))
     ;; A call on the machine stack, so not a safepoint
     ((def-noalloc? def)
      (emit-call! 'stackcall (def-result def) (cons (def-code def) ops)))
     (else
      (let ((f (direct-frame def form)))
        (for-each (lambda (op i) (frame-store! f i op)) ops (iota (length ops)))
        (emit-call! 'call (def-result def) (list (def-code def) f))))))


  ;; Calls allocate the callee's frame first, then store each argument in it as soon as it is
  ;; computed, so only the frame is live across the calls the arguments make
  (define (direct-frame def form)
    (call-def (core-def 'make-direct-frame) (list (def-code def) `(as-val ,(def-proc def)))
              form))


  (define (frame-store! f i op)
    (emit! `(store (as-raw ,f) (quad ,(+ i 6)) ,op)))


  ;; (op dst . ops), op being call or stackcall, for a def with the given result
  (define (emit-call! op result ops)
    (case result
      ((void never)
       (emit! `(,op void ,@ops))
       (if (eq? result 'never)
           (begin (emit! '(unreachable)) '(never))
           '(void)))
      (else
       (emit-op! op result ops))))


  ;; The call of proc with n arguments, (fill! f) storing them in its frame f; when the arity
  ;; is not n, the frame is a placeholder holding them, for apply-frame
  (define (frame-call proc n fill! form tail?)
    (let ((f (call-def (core-def 'make-call-frame) (list proc `(quad ,n)) form))
          (slow (fresh-label)))
      (fill! f)
      ((expressions-inline-branch (current-expressions))
       (core-def 'frame-fits?) (list f `(quad ,n)) form #f slow)
      (if tail?
          (begin
            (emit! `(tailcall frame ,f))
            (start-block! slow)
            (call-def-tail (core-def 'apply-frame) (list f) form))
          (let ((dst (fresh-tmp 'val))
                (join (fresh-label)))
            (emit! `(call ,dst frame ,f))
            (emit! `(jmp ,join))
            (start-block! slow)
            (emit! `(mov ,dst ,(call-def (core-def 'apply-frame) (list f) form)))
            (start-block! join)
            dst))))


  ;; The tailcall of the def-called? def with the operands ops
  (define (call-def-tail def ops form)
    (let ((f (direct-frame def form)))
      (for-each (lambda (op i) (frame-store! f i op)) ops (iota (length ops)))
      (emit! `(tailcall ,(def-code def) ,f))))


  (define (finish-result def op)
    (let ((result (def-result def)))
      (cond
       ((eq? (kind op) 'never)
        op)
       ((eq? result 'never)
        (emit! '(unreachable))
        '(never))
       ((eq? result 'void)
        '(void))
       (else
        (check-kind op result (def-name def)) op))))


  ;; The enclosing named lets, as state (outer or lambda)
  (define (disable-loops env state)
    (map (lambda (entry)
           (if (and (loop-binding? (cdr entry)) (eq? (loop-binding-state (cdr entry)) 'here))
               (cons (car entry) (make-loop-binding #f '() state))
               entry))
         env))


  ;; Stores the captures in the closure c, a raw from alloc-closure
  (define (fill-captures c captures name)
    (for-each (lambda (capture i)
                (call-def (core-def 'closure-set!)
                          (list c `(quad ,i) (local-binding-operand (cdr capture)))
                          name))
              captures (iota (length captures))))


  ;; Compiles the procedure def (a lambda, a procedure define or main) in env, the enclosing
  ;; env, of which it sees the static procedures; its body binds its captures
  (define (compile-procedure def env)
    (let* ((args (def-core-names def))
           (body (def-core-body def))
           (outer (disable-loops (filter (lambda (b) (not (local-binding? (cdr b)))) env) 'lambda))
           (blocks
            (with-function
             (lambda ()
               (parameterize ((unsafe-code? (def-unsafe? def))
                              (current-function def))
                 (let ((body-env (append (map (lambda (x i)
                                                (cons x (make-local-binding `(arg ,(+ i 1)))))
                                              args (iota (length args)))
                                         outer)))
                   (when (string=? (def-symbol def) "main")
                     (for-each (lambda (d) (call-def d '() 'main))
                               (startup-defs)))
                   ((expressions-body (current-expressions)) body-env def body)))))))
      (check-gc-safety (def-name def) blocks)
      (add-entries! (procedure-entries def blocks))))


  ;; The function of def, which fills the literal cells of the compilation at startup, in order:
  ;; a string's with literal-string of its UTF-8 bytes, a static string, a symbol's with
  ;; string->symbol of its name's cell, filled before
  (define (compile-literal-init def)
    (let ((blocks
           (with-function
            (lambda ()
              (parameterize ((unsafe-code? #t)
                             (current-function def))
                (for-each (lambda (l)
                            (let* ((x (car l))
                                   (op (if (symbol? x)
                                           (call-def (core-def 'string->symbol)
                                                     (list (literal-ref (symbol->string x)))
                                                     x)
                                           (call-def (core-def 'literal-string)
                                                     (list (intern-string! x)
                                                           `(quad ,(bytevector-length (string->utf8 x)))
                                                           `(quad ,(string-length x))
                                                           `(quad ,(if (ascii? x) 1 0)))
                                                     x))))
                              (emit! `(store ,(literals-symbol) (quad ,(cdr l)) ,op))))
                          (reverse (current-literals)))
                ((expressions-body (current-expressions)) '() def (list undef)))))))
      (check-gc-safety (def-name def) blocks)
      (add-entries! (procedure-entries def blocks))))


  (define (ascii? s)
    (for-all (lambda (c) (< (char->integer c) 128)) (string->list s)))


  ;; A def with typed parameters, compiled in its env
  (define (compile-low-def def)
    (let* ((params (map cons (def-core-names def) (def-params-kinds def)))
           (blocks
            (with-function
             (lambda ()
               (parameterize ((unsafe-code? (def-unsafe? def))
                              (current-function def))
                 (let ((body-env (map (lambda (p i)
                                        (let ((arg `(arg ,(+ i 1))))
                                          (cons (car p)
                                                (make-local-binding
                                                 (if (eq? (cdr p) 'raw) `(as-raw ,arg) arg)))))
                                      params (iota (length params)))))
                   ((expressions-body (current-expressions)) body-env def (def-core-body def))))))))
      (check-gc-safety (def-name def) blocks)
      (if (def-noalloc? def)
          (begin
            (check-noalloc (def-name def) blocks)
            (add-entries! `((noalloc ,(def-code def) (arity ,(length params))
                                     ,@(lower-function (def-code def) blocks #t)))))
          (add-entries! (procedure-entries def blocks)))))


  ;; A noalloc def may run during a collection, so it never allocates or
  ;; calls Scheme; it has no procedure object, so no self
  (define (check-noalloc name body)
    (define (self? x)
      (and (pair? x)
           (or (equal? x '(self))
               (self? (car x))
               (self? (cdr x)))))

    (for-each (lambda (block)
                (for-each (lambda (instr)
                            (when (or (memq (car instr) '(call tailcall selfcall alloc allocframe))
                                      (self? instr))
                              (error 'check-noalloc "safepoint or self in a noalloc def" name instr)))
                          (cddr block)))
              body))


  ;; A variadic twin stands for its def
  (define (procedure-name def)
    (let ((s (symbol->string (def-name def))))
      (if (and (> (string-length s) 9) (string=? (substring s 0 9) "variadic-"))
          (substring s 9 (string-length s))
          s)))


  ;; The def's name, procedure object and function; a closure def's object is the template
  ;; alloc-closure copies, with its ncaptures
  (define (procedure-entries def blocks)
    (let* ((proc (def-proc def))
           (arity (length (def-params def)))
           (name (and (not (def-anonymous? def)) (string-append proc "_name"))))
      `(,@(if name `((string ,name ,(procedure-name def))) '())
        (rodata ,proc (quad ,procedure-type) (quad ,(if (def-rest? def) (- arity) arity))
                (quad ,(def-code def)) (quad ,(or name 0)) (quad ,(def-captures def)))
        (function ,(def-code def) (arity ,arity)
                  ,@(lower-function (def-code def) blocks #f)))))


  (define (lower-function name blocks noalloc?)
    (let ((head `(function ,name)))
      (dumped 'copy-prop head
              (propagate-copies (dumped 'compile head (map lower-block blocks)) noalloc?)))))
