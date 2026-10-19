;; Scheme to block IR: closures, kinds, tail calls, named lets as loops, inlining.
;;
;; There are 4 kinds:
;; - raw    a plain word
;; - val    a tagged value
;; - never  does not return
;; - void   returns nothing
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

(library (compile)
  (export compile-procedure compile-low-def compile-literal-init)
  (import (chezscheme)
          (utils)
          (parse)
          (vars)
          (closure)
          (defs)
          (gc-check)
          (output)
          (emit)
          (copy-prop))


  ;; The def of the function being compiled, for self tail calls
  (define current-function (make-parameter #f))


  (define (kind x)
    (cond
     ((string? x) 'raw)
     ((memq (car x) '(quad as-raw)) 'raw)
     ((memq (car x) '(raw never void)) (car x))
     ((eq? (car x) 'mutable) (kind (cadr x)))
     (else 'val)))


  (define (check-kind x k expr)
    (unless (eq? (kind x) k)
      (error 'check-kind (format "expected a ~a" k) expr)))


  (define (mutable? op)
    (and (pair? op) (eq? (car op) 'mutable)))


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


  ;; The entry of tests of the low test expr, or #f
  (define (test-form expr)
    (and (eq? (special-keyword expr) 'low) (assq (cadr expr) tests)))


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


  ;; The load of the literal cell of the string or symbol x
  (define (literal-ref x)
    (emit-op! 'load 'val (list (literals-symbol) `(quad ,(intern-literal! x)))))


  (define (compile-variable env x)
    (let ((b (lookup env x)))
      (cond
       ((local-binding? b)
        (let* ((op (local-binding-operand b))
               ;; Copied, so a later set! does not change an operand already computed
               (v (if (mutable? op) (emit-op! 'mov (kind op) (cdr op)) op)))
          (if (local-binding-box? b)
              (call-def env (core-def 'box-ref) (list v) x compile-result)
              v)))
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
    (let ((t (call-def env (core-def 'make-vector)
                       (list (immediate-operand (vector-length v)) (immediate-operand 0))
                       v compile-result)))
      (for-each (lambda (x i)
                  (unless (or (exact-rational? x) (boolean? x) (char? x) (string? x) (symbol? x)
                              (null? x) (vector? x))
                    (error 'compile-value "unsupported literal" x))
                  (call-def env (core-def 'vector-set!)
                            (list t (immediate-operand i)
                                  (compile-value env (cond
                                                      ((null? x) ''())
                                                      ((symbol? x) (make-quoted-symbol x))
                                                      (else x))))
                            v compile-result))
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


  ;; if, let, begin, closure, set!, and when and unless in unsafe defines
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

      ((set!)
       (let-values (((var value) (parse-set! expr)))
         (compile-set! env var value)))

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
                 (call-def env def (compile-args env def expr) expr compile-result))))
       (else
        (compile-indirect env expr #f)))))


  ;; A def reached by a Scheme call, with a frame, rather than inlined or a noalloc call
  (define (def-called? def)
    (not (or (def-inline? def) (def-noalloc? def))))


  ;; Calls def with the operands ops, compiling an inline def's last expression with
  ;; (compile-last env def x)
  (define (call-def env def ops form compile-last)
    (cond
     ((def-inline? def)
      (compile-inline def ops form compile-last))
     ;; A call on the machine stack, so not a safepoint
     ((def-noalloc? def)
      (emit-call! 'stackcall (def-result def) (cons (def-code def) ops)))
     (else
      (let ((f (direct-frame env def form)))
        (for-each (lambda (op i) (frame-store! f i op)) ops (iota (length ops)))
        (emit-call! 'call (def-result def) (list (def-code def) f))))))


  ;; Calls allocate the callee's frame first, then store each argument in it as soon as it is
  ;; computed, so only the frame is live across the calls the arguments make
  (define (direct-frame env def form)
    (call-def env (core-def 'make-direct-frame) (list (def-code def) `(as-val ,(def-proc def)))
              form compile-result))


  (define (frame-store! f i op)
    (emit! `(store (as-raw ,f) (quad ,(+ i 6)) ,op)))


  (define (compile-frame-args! env f exprs form)
    (for-each (lambda (x i) (frame-store! f i (compile-checked env x 'val form)))
              exprs (iota (length exprs))))


  ;; The call (def arg ...) of a def-called? def, a tailcall when tail?
  (define (compile-direct env def expr tail?)
    (let ((f (direct-frame env def expr)))
      (if (def-rest? def)
          (let ((r (def-required def)))
            (compile-frame-args! env f (list-head (cdr expr) r) expr)
            (frame-store! f r (compile-rest-list env (list-tail (cdr expr) r) expr)))
          (compile-frame-args! env f (cdr expr) expr))
      (if tail?
          (emit! `(tailcall ,(def-code def) ,f))
          (emit-call! 'call (def-result def) (list (def-code def) f)))))


  ;; expr is (op arg ...)
  (define (compile-low-op env op expr)
    (cond
     ((assq op tests)
      (compile-if env (cons (marker 'low) expr) #t #f))

     ((assq op binary-ops)
      => (lambda (entry)
           (emit-op! (cdr entry) 'raw (compile-operands env expr '(raw raw)))))

     (else
      (case op
        ((raw)
         (unless (and (= (length expr) 2)
                      (integer? (cadr expr))
                      (exact? (cadr expr))
                      (<= (- (expt 2 63)) (cadr expr) (- (expt 2 64) 1)))
           (error 'compile-value "invalid raw literal" expr))
         `(quad ,(cadr expr)))

        ;; Casts only wrap the operand,
        ;; except that as-val copies a raw slot into a tmp, since the GC scans only tmps
        ((as-raw as-val)
         (unless (= (length expr) 2)
           (error 'compile-value "invalid cast" expr))
         (let ((x (compile-checked env (cadr expr) (if (eq? op 'as-raw) 'val 'raw) expr)))
           (if (and (eq? op 'as-val)
                    (not (zero? (raws-in x))))
               (emit-op! 'mov 'val (list x))
               (list op x))))

        ;; (syscall n arg ...): a Linux system call, raw arguments and result
        ;; (a negative errno on failure), not a safepoint
        ((syscall)
         (unless (and (<= 2 (length expr) 8)
                      (integer? (cadr expr))
                      (exact? (cadr expr))
                      (>= (cadr expr) 0))
           (error 'compile-value "invalid syscall" expr))
         (emit-op! 'syscall 'raw
                   (cons `(quad ,(cadr expr))
                         (compile-exprs env (cddr expr) (map (lambda (x) 'raw) (cddr expr)) expr))))

        ;; (load address index): the raw word at word index of address
        ((load)
         (emit-op! 'load 'raw (compile-operands env expr '(raw raw))))

        ;; (store address index x): sets the word at word index of address to x
        ((store)
         (unless (= (length expr) 4)
           (error 'compile-value "invalid store" expr))
         (let* ((base (compile-checked env (cadr expr) 'raw expr))
                (index (compile-checked env (caddr expr) 'raw expr))
                (x (compile-value env (cadddr expr))))
           (unless (memq (kind x) '(val raw))
             (error 'compile-value "expected a val or a raw" expr))
           (emit! `(store ,base ,index ,x))
           '(void)))

        ;; (load8 address i): the byte at byte index i of address, zero-extended
        ((load8)
         (emit-op! 'load8 'raw (compile-operands env expr '(raw raw))))

        ;; (store8 address i x): sets the byte at byte index i of address to the low byte of x
        ((store8)
         (emit! `(store8 ,@(compile-operands env expr '(raw raw raw))))
         '(void))

        ;; (load32 address i): the 32 bits at byte index i of address, zero-extended
        ((load32)
         (emit-op! 'load32 'raw (compile-operands env expr '(raw raw))))

        ;; (store32 address i x): sets the 32 bits at byte index i of address to the low 32 bits of x
        ((store32)
         (emit! `(store32 ,@(compile-operands env expr '(raw raw raw))))
         '(void))

        ;; (copy-words dst src n): copies n words from src to dst, which do not overlap
        ((copy-words)
         (emit! `(copy ,@(compile-operands env expr '(raw raw raw))))
         '(void))

        ;; (fill-words address n x): sets n words from address to x
        ((fill-words)
         (emit! `(fill ,@(compile-operands env expr '(raw raw raw))))
         '(void))

        ;; (raw/wu hi lo d): the unsigned quotient of hi * 2^64 + lo by d, for hi < d
        ((raw/wu)
         (emit-op! 'divwu 'raw (compile-operands env expr '(raw raw raw))))

        ;; (alloc nwords): the raw address of nwords new zeroed words
        ((alloc)
         (let ((ops (compile-operands env expr '(raw))))
           (when (and (eq? (caar ops) 'quad) (<= (cadar ops) 0))
             (error 'compile-value "invalid alloc" expr))
           (emit-op! 'alloc 'raw ops)))

        ;; (alloc-frame nwords proc): a new frame of nwords zeroed words, its header and self
        ;; set, a val; proc stays a root while it is allocated
        ((alloc-frame)
         (emit-op! 'allocframe 'val (compile-operands env expr '(raw val))))

        ;; (tail-call-frame f): enters the frame f, filled, as a tail call
        ((tail-call-frame)
         (emit! `(tailcall frame ,@(compile-operands env expr '(val))))
         '(never))

        ((self)
         (unless (= (length expr) 1)
           (error 'compile-value "invalid self" expr))
         '(self))

        ;; (address "name"): the address of symbol name
        ((address)
         (unless (and (= (length expr) 2)
                      (string? (cadr expr)))
           (error 'compile-value "invalid address" expr))
         (cadr expr))))))


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
    (fold-right (lambda (op acc) (call-def env (core-def 'cons) (list op acc) form compile-result))
                (immediate-operand ''())
                (compile-exprs env exprs (map (lambda (x) 'val) exprs) form)))


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
            (frame-call env proc n
                        (lambda (f) (for-each (lambda (op i) (frame-store! f i op)) ops (iota n)))
                        expr #t))
          (frame-call env proc n (lambda (f) (compile-frame-args! env f (cdr expr) expr))
                      expr tail?))))


  ;; The call of proc with n arguments, (fill! f) storing them in its frame f; when the arity
  ;; is not n, the frame is a placeholder holding them, for apply-frame
  (define (frame-call env proc n fill! form tail?)
    (let ((f (call-def env (core-def 'make-call-frame) (list proc `(quad ,n)) form
                       compile-result))
          (slow (fresh-label)))
      (fill! f)
      (compile-inline (core-def 'frame-fits?) (list f `(quad ,n)) form
                      (lambda (env def x) (compile-branch env x #f slow)))
      (if tail?
          (begin
            (emit! `(tailcall frame ,f))
            (start-block! slow)
            (call-def-tail env (core-def 'apply-frame) (list f) form))
          (let ((dst (fresh-tmp 'val))
                (join (fresh-label)))
            (emit! `(call ,dst frame ,f))
            (emit! `(jmp ,join))
            (start-block! slow)
            (emit! `(mov ,dst ,(call-def env (core-def 'apply-frame) (list f) form
                                         compile-result)))
            (start-block! join)
            dst))))


  ;; The tailcall of the def-called? def with the operands ops
  (define (call-def-tail env def ops form)
    (let ((f (direct-frame env def form)))
      (for-each (lambda (op i) (frame-store! f i op)) ops (iota (length ops)))
      (emit! `(tailcall ,(def-code def) ,f))))


  ;; Every expression but the last for effect, then (compile-last env last)
  (define (compile-seq env exprs compile-last)
    (let-values (((init last) (split-last exprs)))
      (for-each (lambda (x) (compile-value env x)) init)
      (compile-last env last)))


  ;; The value of a def's last expression x, checked against its result
  (define (compile-result env def x)
    (finish-result def (compile-value env x)))


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
        (for-each (lambda (v)
                    (when (boxed? v)
                      (error 'compile-loop "assigned and captured loop variable" v)))
                  vars)
        (start-block! label)
        (compile-seq (append (map (lambda (v slot)
                                    (cons v (make-local-binding `(mutable ,slot) #f)))
                                  vars slots)
                             (acons name (make-loop-binding label slots 'here) env))
                     body
                     compile-last))))


  ;; Reading a var copies its slot, so the args are computed before any slot is set
  (define (compile-loop-call env x loop)
    (let ((slots (loop-binding-slots loop)))
      (unless (= (length (cdr x)) (length slots))
        (error 'compile-loop-call "wrong number of arguments" x))
      (let ((ops (map-in-order (lambda (arg slot) (compile-checked env arg (kind slot) x))
                               (cdr x) slots)))
        (for-each (lambda (slot op) (emit! `(mov ,slot ,op))) slots ops)
        (emit! `(jmp ,(loop-binding-label loop))))))


  ;; The enclosing named lets, as state (outer or lambda)
  (define (disable-loops env state)
    (map (lambda (entry)
           (if (and (loop-binding? (cdr entry)) (eq? (loop-binding-state (cdr entry)) 'here))
               (cons (car entry) (make-loop-binding #f '() state))
               entry))
         env))


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
           (callee-env (map (lambda (name op) (bind-local name op #f)) names ops)))
      (parameterize ((inlining (cons def (inlining)))
                     (unsafe-code? (def-unsafe? def)))
        (compile-seq callee-env (def-core-body def)
                     (lambda (env x) (compile-last env def x))))))


  ;; The env entry of the variable name bound to op: a box when it is assigned and captured,
  ;; its own slot when it is only assigned (op itself when own?)
  (define (bind-local name op own?)
    (cond
     ((boxed? name)
      (when (eq? (kind op) 'raw)
        (error 'bind-local "captured raw variable" name))
      (cons name (make-local-binding (call-def '() (core-def 'make-box) (list op) name compile-result)
                                     #t)))
     ((not (assigned? name))
      (cons name (make-local-binding op #f)))
     (own?
      (cons name (make-local-binding `(mutable ,op) #f)))
     (else
      (cons name (make-local-binding `(mutable ,(emit-op! 'mov (kind op) (list op))) #f)))))


  (define (compile-let env expr compile-last)
    (let-values (((bindings body) (parse-let expr)))
      (let* ((ops (map-in-order (lambda (b)
                                  (let ((op (compile-named env (car b) (cadr b))))
                                    (unless (memq (kind op) '(val raw))
                                      (error 'compile-let "expected a val or a raw" b))
                                    op))
                                bindings)))
        (compile-seq (append (map (lambda (b op) (bind-local (car b) op #f)) bindings ops)
                             env)
                     body compile-last))))


  ;; expr, naming it name if it is a closure
  (define (compile-named env name expr)
    (if (eq? (special-keyword expr) 'closure)
        (compile-lambda env name expr)
        (compile-value env expr)))


  (define (compile-set! env var expr)
    (let ((b (lookup env var))
          (src (compile-named env var expr)))
      (cond
       ((and (local-binding? b) (local-binding-box? b))
        (call-def env (core-def 'box-set!) (list (local-binding-operand b) src) var compile-result))
       ((and (local-binding? b) (mutable? (local-binding-operand b)))
        (let ((slot (cadr (local-binding-operand b))))
          (check-kind src (kind slot) var)
          (emit! `(mov ,slot ,src))))
       (else
        (error 'compile-set! "set! of a library procedure or primitive" var)))
      (immediate 'undef)))


  ;; Lifts the closure form into its own function, and returns its procedure object, or a
  ;; closure filled with the captures
  (define (compile-lambda env name form)
    (let* ((captures (lambda-captures env form))
           (def (lambda-def name form captures)))
      (compile-procedure def env captures)
      (if (null? captures)
          `(as-val ,(def-proc def))
          (let ((c (call-def env (core-def 'alloc-closure) (list (def-proc def)) name
                             compile-result)))
            (fill-captures env c captures name)
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


  ;; Stores the captures in the closure c, a raw from alloc-closure
  (define (fill-captures env c captures name)
    (for-each (lambda (capture i)
                (let ((op (local-binding-operand (cdr capture))))
                  (call-def env (core-def 'closure-set!)
                            (list c `(quad ,i) (if (mutable? op) (emit-op! 'mov 'val (cdr op)) op))
                            name compile-result)))
              captures (iota (length captures))))


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
      (for-each (lambda (def) (compile-procedure def env '())) defs)
      (if (null? closures)
          (compile-seq env body compile-last)
          (let* ((tmps (map (lambda (name) (fresh-tmp 'val)) names))
                 (inner (append (map (lambda (name tmp) (cons name (make-local-binding tmp #f)))
                                     names tmps)
                                env))
                 (captures (map (lambda (form) (lambda-captures inner form)) forms))
                 (defs (map lambda-def names forms captures)))
            (for-each (lambda (def c) (compile-procedure def inner c)) defs captures)
            (for-each (lambda (def c tmp)
                        (let ((closure (call-def env (core-def 'alloc-closure)
                                                 (list (def-proc def)) (def-name def)
                                                 compile-result)))
                          (for-each (lambda (i)
                                      (call-def env (core-def 'closure-set!)
                                                (list closure `(quad ,i) (immediate 'undef))
                                                (def-name def) compile-result))
                                    (iota (length c)))
                          (emit! `(mov ,tmp (as-val ,closure)))))
                      defs captures tmps)
            (for-each (lambda (def c tmp) (fill-captures inner `(as-raw ,tmp) c (def-name def)))
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
      (call-def env (core-def 'definition-error)
                (list (intern-string! (symbol->string (cadr x)))
                      (intern-string! (if value "assigned" "used")))
                x compile-result)
      (start-block! label-ok)
      (or value op)))


  ;; Compiles the procedure def (a lambda, a procedure define or main) in env, the enclosing
  ;; env, with its captures, (name . binding) pairs of env, loaded from the closure
  (define (compile-procedure def env captures)
    (let* ((args (def-core-names def))
           (body (def-core-body def))
           (outer (disable-loops (filter (lambda (b) (not (local-binding? (cdr b)))) env) 'lambda))
           (blocks
            (with-function
             (lambda ()
               (parameterize ((unsafe-code? (def-unsafe? def))
                              (current-function def))
                 (let* ((inner (append (map (lambda (c i)
                                              (cons (car c)
                                                    (make-local-binding
                                                     (call-def env (core-def 'closure-ref)
                                                               (list `(quad ,i)) (car c)
                                                               compile-result)
                                                     (local-binding-box? (cdr c)))))
                                            captures (iota (length captures)))
                                       outer))
                        (body-env (append (map (lambda (x i) (bind-local x `(arg ,(+ i 1)) #t))
                                               args (iota (length args)))
                                          inner)))
                   (when (string=? (def-symbol def) "main")
                     (for-each (lambda (d) (call-def env d '() 'main compile-result))
                               (startup-defs)))
                   (compile-seq body-env body (lambda (env x) (compile-tail env def x)))))))))
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
                                           (call-def '() (core-def 'string->symbol)
                                                     (list (literal-ref (symbol->string x)))
                                                     x compile-result)
                                           (call-def '() (core-def 'literal-string)
                                                     (list (intern-string! x)
                                                           `(quad ,(bytevector-length (string->utf8 x)))
                                                           `(quad ,(string-length x))
                                                           `(quad ,(if (ascii? x) 1 0)))
                                                     x compile-result))))
                              (emit! `(store ,(literals-symbol) (quad ,(cdr l)) ,op))))
                          (reverse (current-literals)))
                (compile-tail '() def undef))))))
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
                 (let ((body-env (append (map (lambda (p i)
                                                (when (boxed? (car p))
                                                  (error 'compile-low-def
                                                         "assigned and captured parameter" (car p)))
                                                (let ((arg `(arg ,(+ i 1))))
                                                  (bind-local (car p)
                                                              (if (eq? (cdr p) 'raw) `(as-raw ,arg) arg)
                                                              #t)))
                                              params (iota (length params))))))
                   (compile-seq body-env (def-core-body def)
                                (lambda (env x) (compile-tail env def x)))))))))
      (check-gc-safety (def-name def) blocks)
      (if (def-noalloc? def)
          (begin
            (check-noalloc (def-name def) blocks)
            (add-entries! `((noalloc ,(def-code def) (arity ,(length params))
                                     ,@(lower-function (def-code def) blocks #t)))))
          (add-entries! (procedure-entries def blocks)))))


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
               (emit! `(,(if jump-if (cadr entry) (caddr entry)) ,label)))))

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
                                    (call-def env def (compile-args env def test) test
                                              compile-result))
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
          (if (eq? (kind a) 'never) b dst)))))


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
