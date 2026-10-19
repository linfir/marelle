;; Expressions to block IR: kinds, tail calls, named lets as loops, inlining, closures.
;;
;; An expression is compiled either
;; - as a value
;;   - compile-value, giving an operand
;;   - compile-checked, same but checking the kind
;;   - compile-result, the last value of a def, checked against its result
;; - in exit position, ending an evaluation
;;   - compile-exit, through if, let, begin, named lets and the forms expanding to them;
;;     a call to a named let there jumps back to it
;; - in tail position
;;   - compile-tail, an exit where a call becomes a tailcall, any other value is returned
;; - as a branch
;;   - compile-branch, giving a conditional jump

(library (expr)
  (export scheme-expressions)
  (import (chezscheme)
          (utils)
          (parse)
          (vars)
          (closure)
          (defs)
          (ops)
          (gc-check)
          (output)
          (emit)
          (compile))


  (define scheme-expressions
    (make-expressions
     (lambda (def ops form)
       (compile-inline def ops form compile-result))
     (lambda (def ops form jump-if label)
       (compile-inline def ops form (lambda (env def x) (compile-branch env x jump-if label))))
     (lambda (env def exprs)
       (compile-seq env exprs (lambda (env x) (compile-tail env def x))))))


  (define (lookup env x)
    (let ((entry (assq x env)))
      (unless entry
        (error 'lookup "unknown symbol" x))
      (cdr entry)))


  ;; The binding of the head of the call expr, or #f
  (define (head-binding env expr)
    (and (non-empty-list? expr)
         (cond
          ((symbol? (car expr))
           (let ((entry (assq (car expr) env)))
             (and entry (cdr entry))))
          ((def? (car expr))
           (make-def-binding (car expr)))
          (else #f))))


  ;; The jumps of the low test expr, (jump-if-true jump-if-false), or #f
  (define (test-form expr)
    (and (eq? (special-keyword expr) 'low) (low-test (cadr expr))))


  ;; The def a call expr calls with the right number of arguments, or #f; another number goes
  ;; to the def's variadic twin, if any; else it is an error, except for a procedure, which
  ;; checks it when called
  (define (called-def env expr)
    (let ((b (head-binding env expr))
          (n (length (cdr expr))))
      (and (def-binding? b)
           (let ((def (def-binding-def b)))
             (cond
              ((takes? def n) def)
              ((variadic-twin def) => (lambda (twin) (and (takes? twin n) twin)))
              ((def-procedure? def) #f)
              (else (error 'compile-call "wrong number of arguments" expr)))))))


  (define (takes? def n)
    (if (def-rest? def)
        (>= n (def-required def))
        (= n (length (def-params def)))))


  (define (immediate-operand x)
    (immediate (cond ((eq? x undef) 'undef) ((eq? x unbound) 'unbound) (else x))))


  ;; Emits the code of expr and returns its operand, one of:
  ;; - a slot: (val n), (raw n), (arg n), (self)
  ;; - a constant: (quad n), or a string, the address of a symbol (a C string in unsafe code)
  ;; - a marker: (void), (never)
  ;; - a cast: (as-val x) or (as-raw x), only for kind, removed by lower-operand
  (define (compile-value env expr)
    (cond
     ((immediate? expr)
      (immediate-operand expr))

     ((exact-rational? expr)
      `(as-val ,(intern-number! expr)))

     ((number? expr)
      (error 'compile-value "unsupported number" expr))

     ;; Outside unsafe code, a Scheme string, loaded from its literal cell
     ((string? expr)
      (if (unsafe-code?)
          (intern-string! expr)
          (literal-ref expr)))

     ((quoted-symbol? expr)
      (literal-ref (quoted-symbol-name expr)))

     ((symbol? expr)
      (compile-variable env expr))

     ((def? expr)
      (def-value expr (def-name expr)))

     ;; A data name gives the raw address of its first word
     ((data? expr)
      (data-symbol expr))

     ((vector-literal? expr)
      (compile-vector-literal env (if (vector? expr) expr (cadr expr))))

     ((expand-form env expr)
      => (lambda (x) (compile-value env x)))

     ((special-keyword expr)
      => (lambda (keyword) (compile-special env keyword expr)))

     ((non-empty-list? expr)
      (compile-call env expr))

     (else
      (error 'compile-value "unsupported expression" expr))))


  (define (compile-variable env x)
    (let ((b (lookup env x)))
      (cond
       ((local-binding? b)
        (local-binding-operand b))
       ;; A static procedure of a fix
       ((def-binding? b)
        (def-value (def-binding-def b) x))
       (else
        (loop-error b x)))))


  (define (loop-error b x)
    (if (eq? (loop-binding-state b) 'lambda)
        (error 'compile-value "named let called from a lambda" x)
        (error 'compile-value "named let used as a value or called not in exit position" x)))


  ;; The procedure object of def, or of its variadic twin, which takes any number of arguments
  (define (def-value def x)
    (let ((def (or (variadic-twin def) def)))
      (unless (def-emitted? def)
        (error 'compile-value "inline def without a procedure object" x))
      (when (def-noalloc? def)
        (error 'compile-value "noalloc def without a procedure object" x))
      `(as-val ,(def-proc def))))


  ;; A vector literal, rebuilt at each evaluation like bignums, so it is never static;
  ;; its elements are numbers, booleans, characters, strings, symbols, () and vectors
  (define (compile-vector-literal env v)
    (let ((t (call-def (core-def 'make-vector)
                       (list (immediate-operand (vector-length v)) (immediate-operand 0))
                       v)))
      (for-each (lambda (x i)
                  (unless (or (exact-rational? x) (boolean? x) (char? x) (string? x) (symbol? x)
                              (null? x) (vector? x))
                    (error 'compile-value "unsupported literal" x))
                  (call-def (core-def 'vector-set!)
                            (list t (immediate-operand i)
                                  (compile-value env (cond
                                                      ((null? x) ''())
                                                      ((symbol? x) (make-quoted-symbol x))
                                                      (else x))))
                            v))
                (vector->list v) (iota (vector-length v)))
      t))


  ;; and and or as values, as if, let and begin, or #f
  (define (expand-form env expr)
    (case (special-keyword expr)
      ;; In a begin, since #f means no expansion
      ((and) `(,(marker 'begin) ,(expand-and (cdr expr))))
      ((or) `(,(marker 'begin) ,(expand-or (cdr expr))))
      (else #f)))


  (define (expand-or exprs)
    (cond
     ((null? exprs) #f)
     ((null? (cdr exprs)) (car exprs))
     (else
      (let ((t (gensym "or")))
        `(,(marker 'let) ((,t ,(car exprs)))
          (,(marker 'if) ,t ,t ,(expand-or (cdr exprs))))))))


  ;; if, let, begin, closure, the cells, and when and unless in unsafe defines
  (define (compile-special env keyword expr)
    (case keyword
      ((if)
       (let-values (((test then else) (parse-if expr)))
         (compile-if env test then else)))

      ((let)
       (if (named-let? expr)
           (compile-loop-value env expr)
           (compile-let env expr compile-value)))

      ((begin)
       (compile-seq env (parse-begin expr) compile-value))

      ((closure)
       (compile-lambda env #f expr))

      ;; A cell is a slot, its reads copied, so a later %cell-set! does not change an operand
      ;; already computed
      ((%cell)
       (let ((op (compile-value env (cadr expr))))
         (unless (memq (kind op) '(val raw))
           (error 'compile-value "expected a val or a raw" expr))
         (emit-op! 'mov (kind op) (list op))))

      ((%cell-ref)
       (let ((c (compile-value env (cadr expr))))
         (emit-op! 'mov (kind c) (list c))))

      ((%cell-set!)
       (let* ((c (compile-value env (cadr expr)))
              (op (compile-value env (caddr expr))))
         (check-kind op (kind c) (cadr expr))
         (emit! `(mov ,c ,op))
         '(void)))

      ((fix)
       (compile-fix env expr compile-value))

      ((check-defined)
       (compile-check-defined env expr))

      ((const)
       (cadr expr))

      ((low)
       (compile-low-op env (cadr expr) (cdr expr)))

      ;; Only in unsafe defines: the body's value when it is a val, else undef, so the body may
      ;; have any kind
      ((when unless)
       (unless (>= (length expr) 3)
         (error 'compile-value "invalid when or unless" expr))
       (let ((label-skip (fresh-label)))
         (compile-branch env (cadr expr) (eq? keyword 'unless) label-skip)
         (let ((op (compile-seq env (cddr expr) compile-value)))
           (if (eq? (kind op) 'val)
               (let ((dst (fresh-tmp 'val))
                     (label-end (fresh-label)))
                 (emit! `(mov ,dst ,op))
                 (emit! `(jmp ,label-end))
                 (start-block! label-skip)
                 (emit! `(mov ,dst ,(immediate 'undef)))
                 (start-block! label-end)
                 dst)
               (begin
                 (start-block! label-skip)
                 (immediate 'undef))))))

      (else
       (error 'compile-value "unsupported form" expr))))


  (define (compile-call env expr)
    (let ((b (head-binding env expr)))
      (cond
       ((loop-binding? b)
        (loop-error b expr))
       ((called-def env expr)
        => (lambda (def)
             (if (def-called? def)
                 (compile-direct env def expr #f)
                 (call-def def (compile-args env def expr) expr))))
       (else
        (compile-indirect env expr #f)))))


  (define (compile-frame-args! env f exprs form)
    (for-each (lambda (x i) (frame-store! f i (compile-checked env x 'val form)))
              exprs (iota (length exprs))))


  ;; The call (def arg ...) of a def-called? def, a tailcall when tail?
  (define (compile-direct env def expr tail?)
    (let ((f (direct-frame def expr)))
      (if (def-rest? def)
          (let ((r (def-required def)))
            (compile-frame-args! env f (list-head (cdr expr) r) expr)
            (frame-store! f r (compile-rest-list env (list-tail (cdr expr) r) expr)))
          (compile-frame-args! env f (cdr expr) expr))
      (if tail?
          (emit! `(tailcall ,(def-code def) ,f))
          (emit-call! 'call (def-result def) (list (def-code def) f)))))


  ;; expr is (name arg ...), compiled after its entry in the table of ops.sls
  (define (compile-low-op env name expr)
    (let ((op (low-op name)))
      (define (literal? ok?)
        (and (= (length expr) 2) (ok? (cadr expr))))

      (cond
       ((op-test? op)
        (compile-if env (cons (marker 'low) expr) #t #f))

       (else
        (case name
          ((raw)
           (unless (literal? (lambda (n)
                               (and (integer? n) (exact? n) (<= (- (expt 2 63)) n (- (expt 2 64) 1)))))
             (error 'compile-value "invalid raw literal" expr))
           `(quad ,(cadr expr)))

          ((as-raw as-val)
           (let ((x (car (compile-operands env expr (op-operands op)))))
             (if (and (eq? name 'as-val)
                      (not (zero? (raws-in x))))
                 (emit-op! 'mov 'val (list x))
                 (list name x))))

          ((syscall)
           (unless (and (<= 2 (length expr) 8)
                        (integer? (cadr expr))
                        (exact? (cadr expr))
                        (>= (cadr expr) 0))
             (error 'compile-value "invalid syscall" expr))
           (emit-op! (op-instr op) 'raw
                     (cons `(quad ,(cadr expr))
                           (compile-exprs env (cddr expr) (map (lambda (x) 'raw) (cddr expr)) expr))))

          ((alloc)
           (let ((ops (compile-operands env expr (op-operands op))))
             (when (and (eq? (caar ops) 'quad) (<= (cadar ops) 0))
               (error 'compile-value "invalid alloc" expr))
             (emit-op! (op-instr op) 'raw ops)))

          ((tail-call-frame)
           (emit! `(tailcall frame ,@(compile-operands env expr (op-operands op))))
           '(never))

          ((self)
           (compile-operands env expr (op-operands op))
           '(self))

          ((address)
           (unless (literal? string?)
             (error 'compile-value "invalid address" expr))
           (cadr expr))

          (else
           (let ((ops (compile-operands env expr (op-operands op))))
             (if (eq? (op-result op) 'void)
                 (begin
                   (emit! `(,(op-instr op) ,@ops))
                   '(void))
                 (emit-op! (op-instr op) (op-result op) ops)))))))))


  (define (compile-checked env x k form)
    (let ((op (compile-value env x)))
      (check-kind op k form)
      op))


  (define (compile-operands env expr kinds)
    (compile-exprs env (cdr expr) kinds expr))


  ;; form is the whole expression, for errors
  (define (compile-exprs env exprs kinds form)
    (unless (= (length exprs) (length kinds))
      (error 'compile-exprs "wrong number of operands" form))
    (map-in-order (lambda (x k) (compile-checked env x k form)) exprs kinds))


  ;; The operands of a call to def, the extra ones packed into its rest parameter's list
  (define (compile-args env def expr)
    (if (def-rest? def)
        (let ((required (list-head (cdr expr) (def-required def))))
          (append (compile-exprs env required (map (lambda (x) 'val) required) expr)
                  (list (compile-rest-list env (list-tail (cdr expr) (def-required def)) expr))))
        (compile-operands env expr (def-params-kinds def))))


  ;; A fresh list of the values of exprs
  (define (compile-rest-list env exprs form)
    (fold-right (lambda (op acc) (call-def (core-def 'cons) (list op acc) form))
                (immediate-operand ''())
                (compile-exprs env exprs (map (lambda (x) 'val) exprs) form)))


  ;; The call of the procedure value (car expr), a tailcall when tail?; a tail call that may
  ;; be a self call computes its arguments first, so that it can reuse the frame, any other
  ;; call allocates the frame first
  (define (compile-indirect env expr tail?)
    (let ((n (length (cdr expr)))
          (proc (compile-checked env (car expr) 'val expr))
          (self (current-function)))
      (if (and tail? (not (def-rest? self)) (= n (length (def-params self))))
          (let ((ops (compile-exprs env (cdr expr) (map (lambda (x) 'val) (cdr expr)) expr))
                (other (fresh-label)))
            (compile-inline (core-def 'same-code?) (list proc (def-code self)) expr
                            (lambda (env def x) (compile-branch env x #f other)))
            (emit! `(selfcall ,proc ,@ops))
            (start-block! other)
            (frame-call proc n
                        (lambda (f) (for-each (lambda (op i) (frame-store! f i op)) ops (iota n)))
                        expr #t))
          (frame-call proc n (lambda (f) (compile-frame-args! env f (cdr expr) expr))
                      expr tail?))))


  ;; Every expression but the last for effect, then (compile-last env last)
  (define (compile-seq env exprs compile-last)
    (let-values (((init last) (split-last exprs)))
      (for-each (lambda (x) (compile-value env x)) init)
      (compile-last env last)))


  ;; The value of a def's last expression x, checked against its result
  (define (compile-result env def x)
    (finish-result def (compile-value env x)))


  ;; Compiles x, whose value ends an evaluation: through if, let, begin, named lets and the
  ;; forms expanding to them, a call to a named let jumps back to it,
  ;; and any other expression goes to (leaf env x)
  (define (compile-exit env x leaf)
    (define (exit env x)
      (compile-exit env x leaf))

    (let ((b (head-binding env x)))
      (cond
       ((loop-binding? b)
        (unless (eq? (loop-binding-state b) 'here)
          (loop-error b x))
        (compile-loop-call env x b))

       ((expand-form env x)
        => (lambda (x) (exit env x)))

       (else
        (case (special-keyword x)
          ((if)
           (let-values (((test then else) (parse-if x)))
             (let ((label-false (fresh-label)))
               (compile-branch env test #f label-false)
               (exit env then)
               (start-block! label-false)
               (exit env else))))

          ((let)
           (if (named-let? x)
               (compile-loop env x exit)
               (compile-let env x exit)))

          ((begin)
           (compile-seq env (parse-begin x) exit))

          ((fix)
           (compile-fix env x exit))

          (else
           (leaf env x)))))))


  ;; Ends the function with x: a call to a non-inline def becomes a tailcall,
  ;; any other value is returned; def is the innermost def x ends, for its result
  (define (compile-tail env def x)
    (compile-exit
     env x
     (lambda (env x)
       (let ((b (head-binding env x)))
         (cond
          ((or (not (non-empty-list? x))
               (special? x)
               (immediate? x)
               (vector-literal? x))
           (compile-return env def x))

          ((called-def env x)
           => (lambda (callee)
                (cond
                 ((def-noalloc? callee)
                  (compile-return env def x))
                 ((eq? callee (current-function))
                  (emit! `(selfcall (self) ,@(compile-args env callee x))))
                 ((not (def-inline? callee))
                  (compile-direct env callee x #t))
                 ((and (eq? (def-result def) 'val) (eq? (def-result callee) 'val))
                  (compile-inline callee (compile-args env callee x) x
                                  (lambda (env callee x) (compile-tail env callee x))))
                 (else
                  (compile-return env def x)))))

          (else
           (compile-indirect env x #t)))))))


  (define (compile-return env def x)
    (let ((op (compile-result env def x)))
      (unless (eq? (kind op) 'never)
        (emit! `(ret ,op)))))


  ;; (let name ((var init) ...) body ...): each var gets its own slot, then the body
  ;; starts a block; a call (name arg ...) in exit position sets the slots and jumps back
  (define (compile-loop env expr compile-last)
    (let-values (((name bindings body) (parse-named-let expr)))
      (let* ((vars (map car bindings))
             (slots (map-in-order (lambda (b)
                                    (let ((op (compile-value env (cadr b))))
                                      (unless (memq (kind op) '(val raw))
                                        (error 'compile-loop "expected a val or a raw" b))
                                      (let ((slot (fresh-tmp (kind op))))
                                        (emit! `(mov ,slot ,op))
                                        slot)))
                                  bindings))
             (label (fresh-label)))
        (start-block! label)
        (compile-seq (append (map (lambda (v slot) (cons v (make-local-binding slot))) vars slots)
                             (acons name (make-loop-binding label slots 'here) env))
                     body
                     compile-last))))


  ;; The args are computed before any slot is set, an operand reading a slot copied first
  (define (compile-loop-call env x loop)
    (let ((slots (loop-binding-slots loop)))
      (unless (= (length (cdr x)) (length slots))
        (error 'compile-loop-call "wrong number of arguments" x))
      (let ((ops (map (lambda (op)
                        (if (exists (lambda (slot) (reads? op slot)) slots)
                            (emit-op! 'mov (kind op) (list op))
                            op))
                      (map-in-order (lambda (arg slot) (compile-checked env arg (kind slot) x))
                                    (cdr x) slots))))
        (for-each (lambda (slot op) (emit! `(mov ,slot ,op))) slots ops)
        (emit! `(jmp ,(loop-binding-label loop))))))


  (define (reads? op slot)
    (or (equal? op slot)
        (and (pair? op) (exists (lambda (x) (reads? x slot)) (cdr op)))))


  ;; A named let as a value: each exit stores its value and jumps to the end;
  ;; the enclosing named lets are not in exit position
  (define (compile-loop-value env expr)
    (let ((dst #f)
          (label-end (fresh-label)))
      (compile-loop (disable-loops env 'outer)
                    expr
                    (lambda (env x)
                      (compile-exit
                       env x
                       (lambda (env x)
                         (let ((op (compile-value env x)))
                           (unless (eq? (kind op) 'never)
                             (if dst
                                 (check-kind op (kind dst) expr)
                                 (set! dst (if (memq (kind op) '(val raw)) (fresh-tmp (kind op)) op)))
                             (when (memq (kind dst) '(val raw))
                               (emit! `(mov ,dst ,op)))
                             (emit! `(jmp ,label-end))))))))
      (start-block! label-end)
      (or dst '(never))))


  ;; The body of def with its parameters bound to ops, in its own env;
  ;; (compile-last env def x) compiles the last expression
  (define (compile-inline def ops form compile-last)
    (when (memq def (inlining))
      (error 'compile-inline "recursive inline def" (def-name def)))
    (let* ((names (def-core-names def))
           (callee-env (map (lambda (name op) (cons name (make-local-binding op))) names ops)))
      (parameterize ((inlining (cons def (inlining)))
                     (unsafe-code? (def-unsafe? def)))
        (compile-seq callee-env (def-core-body def)
                     (lambda (env x) (compile-last env def x))))))


  (define (compile-let env expr compile-last)
    (let-values (((bindings body) (parse-let expr)))
      (let* ((ops (map-in-order (lambda (b)
                                  (let ((op (compile-named env (car b) (cadr b))))
                                    (unless (memq (kind op) '(val raw))
                                      (error 'compile-let "expected a val or a raw" b))
                                    op))
                                bindings)))
        (compile-seq (append (map (lambda (b op) (cons (car b) (make-local-binding op)))
                                  bindings ops)
                             env)
                     body compile-last))))


  ;; expr, naming it name if it is a closure
  (define (compile-named env name expr)
    (if (eq? (special-keyword expr) 'closure)
        (compile-lambda env name expr)
        (compile-value env expr)))


  ;; Lifts the closure form into its own function, and returns its procedure object, or a
  ;; closure filled with the captures
  (define (compile-lambda env name form)
    (let* ((captures (lambda-captures env form))
           (def (lambda-def name form captures)))
      (compile-procedure def env)
      (if (null? captures)
          `(as-val ,(def-proc def))
          (let ((c (call-def (core-def 'alloc-closure) (list (def-proc def)) name)))
            (fill-captures c captures name)
            (emit-op! 'mov 'val (list `(as-val ,c)))))))


  ;; The entries of env a closure form captures
  (define (lambda-captures env form)
    (map (lambda (var)
           (let ((entry (assq var env)))
             (unless (and entry (local-binding? (cdr entry)))
               (error 'compile-lambda "capture not a local" var))
             entry))
         (cadr form)))


  ;; The def of a closure form, with its symbols
  (define (lambda-def name form captures)
    (let-values (((capture-names args body) (parse-closure form)))
      (let ((def (make-def (or name 'lambda) (mangle (fresh-name (or name 'lambda)))
                           (formals->params args) 'val
                           #f #f #f #t (unsafe-code?) (length captures) (not name) #f body
                           #f (cons (formals-names args) body))))
        (for-each (lambda (c)
                    (when (eq? (kind (local-binding-operand (cdr c))) 'raw)
                      (error 'compile-lambda "captured raw variable" (car c))))
                  captures)
        (add-def-symbols! def)
        def)))


  ;; (fix ((name closure) ...) body ...), from the letrec pass: unassigned names bound to
  ;; lambdas, which may refer to each other. The names of those capturing nothing are static
  ;; procedures, called directly. The others are closures: all allocated, their captures set
  ;; to undef so a collection finds no garbage, then filled.
  (define (compile-fix env x compile-last)
    (let* ((static (filter (lambda (b) (null? (cadadr b))) (cadr x)))
           (defs (map (lambda (b) (lambda-def (car b) (cadr b) '())) static))
           (env (append (map (lambda (b def) (cons (car b) (make-def-binding def))) static defs)
                        env))
           (closures (filter (lambda (b) (pair? (cadadr b))) (cadr x)))
           (names (map car closures))
           (forms (map cadr closures))
           (body (cddr x)))
      (for-each (lambda (def) (compile-procedure def env)) defs)
      (if (null? closures)
          (compile-seq env body compile-last)
          (let* ((tmps (map (lambda (name) (fresh-tmp 'val)) names))
                 (inner (append (map (lambda (name tmp) (cons name (make-local-binding tmp)))
                                     names tmps)
                                env))
                 (captures (map (lambda (form) (lambda-captures inner form)) forms))
                 (defs (map lambda-def names forms captures)))
            (for-each (lambda (def) (compile-procedure def inner)) defs)
            (for-each (lambda (def c tmp)
                        (let ((closure (call-def (core-def 'alloc-closure)
                                                 (list (def-proc def)) (def-name def))))
                          (for-each (lambda (i)
                                      (call-def (core-def 'closure-set!)
                                                (list closure `(quad ,i) (immediate 'undef))
                                                (def-name def)))
                                    (iota (length c)))
                          (emit! `(mov ,tmp (as-val ,closure)))))
                      defs captures tmps)
            (for-each (lambda (def c tmp) (fill-captures `(as-raw ,tmp) c (def-name def)))
                      defs captures tmps)
            (compile-seq inner body compile-last)))))


  ;; (check-defined x read [value]): a read of a letrec variable x that may come before its
  ;; init; with a value, from a set! of it, the value, then the check
  (define (compile-check-defined env x)
    (let* ((value (and (pair? (cdddr x)) (compile-checked env (cadddr x) 'val x)))
           (op (compile-value env (caddr x)))
           (label-ok (fresh-label)))
      (emit! `(cmp ,op ,(immediate 'unbound)))
      (emit! `(jne ,label-ok))
      (call-def (core-def 'definition-error)
                (list (intern-string! (symbol->string (cadr x)))
                      (intern-string! (if value "assigned" "used")))
                x)
      (start-block! label-ok)
      (or value op)))


  ;; and/or in a test: short is the truth that ends the evaluation early
  (define (compile-logic-branch env keyword ops jump-if label)
    (let ((short (eq? keyword 'or)))
      (cond
       ((null? ops)
        (unless (eq? jump-if short)
          (emit! `(jmp ,label))))
       ((eq? jump-if short)
        (for-each (lambda (x) (compile-branch env x short label)) ops))
       (else
        (let ((label-skip (fresh-label)))
          (let-values (((init last) (split-last ops)))
            (for-each (lambda (x) (compile-branch env x short label-skip)) init)
            (compile-branch env last jump-if label))
          (start-block! label-skip))))))


  ;; The library's not, called with one argument
  (define (not-call? env test)
    (let ((b (head-binding env test)))
      (and (def-binding? b)
           (assq 'not (core-defs))
           (eq? (def-binding-def b) (core-def 'not))
           (= (length test) 2))))


  ;; Jumps to label when the truth of test equals jump-if
  (define (compile-branch env test jump-if label)
    (let ((keyword (special-keyword test)))
      (cond
       ((memq keyword '(and or))
        (compile-logic-branch env keyword (cdr test) jump-if label))

       ;; Only built by case: two vals compared as words
       ((eq? keyword 'eq?)
        (let ((ops (compile-exprs env (cdr test) '(val val) test)))
          (emit! `(cmp ,@ops))
          (emit! `(,(if jump-if 'je 'jne) ,label))))

       ((not-call? env test)
        (compile-branch env (cadr test) (not jump-if) label))

       ((test-form test)
        => (lambda (entry)
             (let ((ops (compile-exprs env (cddr test) '(raw raw) (cdr test))))
               (emit! `(cmp ,@ops))
               (emit! `(,(if jump-if (car entry) (cadr entry)) ,label)))))

       ((boolean? test)
        (when (eq? test jump-if)
          (emit! `(jmp ,label))
          (start-block! (fresh-label))))

       ((expand-form env test)
        => (lambda (x) (compile-branch env x jump-if label)))

       ((eq? keyword 'if)
        (let-values (((test then else) (parse-if test)))
          (let ((label-else (fresh-label))
                (label-end (fresh-label)))
            (compile-branch env test #f label-else)
            (compile-branch env then jump-if label)
            (emit! `(jmp ,label-end))
            (start-block! label-else)
            (compile-branch env else jump-if label)
            (start-block! label-end))))

       ((and (eq? keyword 'let) (not (named-let? test)))
        (compile-let env test (lambda (env x) (compile-branch env x jump-if label))))

       ((eq? keyword 'begin)
        (compile-seq env (parse-begin test)
                     (lambda (env x) (compile-branch env x jump-if label))))

       ((and (not keyword)
             (non-empty-list? test)
             (called-def env test))
        => (lambda (def)
             (if (and (def-inline? def) (eq? (def-result def) 'val))
                 (compile-inline def (compile-args env def test) test
                                 (lambda (env def x) (compile-branch env x jump-if label)))
                 (compare-false (if (def-called? def)
                                    (compile-direct env def test #f)
                                    (call-def def (compile-args env def test) test))
                                test jump-if label))))

       (else
        (compare-false (compile-value env test) test jump-if label)))))


  (define (compare-false op test jump-if label)
    (check-kind op 'val test)
    (emit! `(cmp ,op ,(immediate #f)))
    (emit! `(,(if jump-if 'jne 'je) ,label)))


  (define (compile-if env test then else)
    (let* ((label-false (fresh-label))
           (label-end (fresh-label)))
      (compile-branch env test #f label-false)
      ;; A branch that does not return takes the kind of the other one
      (let* ((a (compile-value env then))
             (slot? (memq (kind a) '(val raw)))
             (dst (if slot? (fresh-tmp (kind a)) a)))
        (unless (eq? (kind a) 'never)
          (when slot?
            (emit! `(mov ,dst ,a)))
          (emit! `(jmp ,label-end)))
        (start-block! label-false)
        (let ((b (compile-value env else)))
          (unless (or (eq? (kind a) 'never) (eq? (kind b) 'never))
            (check-kind b (kind a) `(if ,test ,then ,else))
            (when slot?
              (emit! `(mov ,dst ,b))))
          (start-block! label-end)
          (if (eq? (kind a) 'never) b dst))))))
