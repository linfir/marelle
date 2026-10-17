;; Two passes: every def is parsed, then the defs are compiled and printed.

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
;;   - compile-exit, through if, let, let*, begin and named lets;
;;     a call to a named let there jumps back to it
;; - in tail position
;;   - compile-tail, an exit where a call becomes a tailcall, any other value is returned
;; - as a branch
;;   - compile-branch, giving a conditional jump

(library (low-compile)
  (export compile-low)
  (import (chezscheme)
          (utils)
          (low-gc)
          (low-output)
          (low-parse)
          (low-emit))


  ;; Header of procedure objects, procedure-type in runtime.ss
  (define procedure-type (* 42 8))


  ;; The (name . def) alist of the program
  (define current-defs (make-parameter '()))
  ;; The (name . data) alist of the program
  (define current-datas (make-parameter '()))
  ;; The (string . name) alist of the interned strings, newest first
  (define current-strings (make-parameter '()))
  (define inlining (make-parameter '()))


  (define (intern-string! x)
    (cond
     ((assoc x (current-strings)) => cdr)
     (else
      (let ((name (format "str~a" (+ 1 (length (current-strings))))))
        (current-strings (acons x name (current-strings)))
        name))))


  (define (kind x)
    (cond
     ((string? x) 'raw)
     ((memq (car x) '(quad as-raw)) 'raw)
     ((memq (car x) '(raw never void)) (car x))
     (else 'val)))


  (define (check-kind x k expr)
    (unless (eq? (kind x) k)
      (error 'check-kind (format "expected a ~a" k) expr)))


  (define (lookup-def name)
    (let ((entry (assq name (current-defs))))
      (and entry (cdr entry))))


  (define (lookup-var env x)
    (let ((entry (assq x env)))
      (if entry
          (cdr entry)
          (error 'lookup-var "unbound variable" x))))


  (define (test-form? expr)
    (and (pair? expr)
         (assq (car expr) tests)))


  (define (binary-op? expr)
    (and (pair? expr)
         (assq (car expr) binary-ops)))


  (define (call-form? expr)
    (and (pair? expr)
         (symbol? (car expr))
         (lookup-def (car expr))))


  (define (inline-call? expr)
    (let ((def (call-form? expr)))
      (and def (def-inline? def) def)))


  (define (indirect-call? expr)
    (and (pair? expr)
         (not (and (symbol? (car expr))
                   (or (memq (car expr) keywords)
                       (lookup-def (car expr)))))))


  ;; Emits the code of expr and returns its operand, one of:
  ;; - a slot: (val n), (raw n), (arg n), (self)
  ;; - a constant: (quad n), or a string, the address of a symbol
  ;; - a marker: (void), (never)
  ;; - a cast: (as-val x) or (as-raw x), only for kind, removed by lower-operand
  (define (compile-value env expr)
    (cond
     ((immediate expr))

     ;; A data name gives the raw address of its first word
     ((and (symbol? expr)
           (assq expr (current-datas)))
      => (lambda (entry) (data-symbol (cdr entry))))

     ((and (symbol? expr)
           (not (assq expr env))
           (lookup-def expr))
      => (lambda (def)
           (unless (def-emitted? def)
             (error 'compile-value "inline def without a procedure object" expr))
           (when (def-native? def)
             (error 'compile-value "native def without a procedure object" expr))
           ;; A closure def gives the raw address of its template, for alloc-closure
           (if (zero? (def-captures def))
               `(as-val ,(def-proc def))
               (def-proc def))))

     ((symbol? expr)
      (let ((op (lookup-var env expr)))
        (when (and (pair? op) (memq (car op) '(loop outer-loop)))
          (error 'compile-value "named let used as a value or called not in exit position" expr))
        ;; Copied, so a later set! does not change an operand already computed
        (if (and (pair? op) (eq? (car op) 'mutable))
            (emit-op! 'mov (kind (cadr op)) (cdr op))
            op)))

     ((string? expr)
      (intern-string! expr))

     ((test-form? expr)
      (compile-value env `(if ,expr #t #f)))

     ((call-form? expr)
      => (lambda (def)
           (cond
            ((def-inline? def)
             (compile-inline env def expr (lambda (env x) (compile-result env def x))))
            ;; A System V call, so not a safepoint
            ((def-native? def)
             (emit-call! 'ccall (def-result def) (cons (def-code def) (compile-args env def expr))))
            (else
             (emit-call! 'call (def-result def) (compile-direct env def expr))))))

     ((binary-op? expr)
      => (lambda (entry)
           (emit-op! (cdr entry) 'raw (compile-operands env expr '(raw raw)))))

     ((indirect-call? expr)
      (emit-op! 'call 'val (compile-indirect env expr)))

     ((and (pair? expr)
           (symbol? (car expr)))
      (case (car expr)
        ((if)
         (let-values (((test then else) (if-parts expr)))
           (compile-if env test then else)))

        ((let)
         (if (named-let? expr)
             (compile-loop-value env expr)
             (compile-let env expr compile-value)))

        ((let*)
         (compile-value env (let*->let expr)))

        ((begin)
         (compile-seq env (begin-body expr) compile-value))

        ;; (set! x expr): x is a local variable
        ((set!)
         (unless (and (= (length expr) 3) (symbol? (cadr expr)))
           (error 'compile-value "invalid set!" expr))
         (let ((var (lookup-var env (cadr expr))))
           (assert (eq? (car var) 'mutable))
           (let ((slot (cadr var)))
             (emit! `(mov ,slot ,(compile-checked env (caddr expr) (kind slot) expr)))
             '(void))))

        ((when unless)
         (unless (>= (length expr) 3)
           (error 'compile-value "invalid when or unless" expr))
         ;; The body for effect, so it may have any kind
         (let ((label-skip (fresh-label)))
           (compile-branch env (cadr expr) (eq? (car expr) 'unless) label-skip)
           (for-each (lambda (x) (compile-value env x)) (cddr expr))
           (start-block! label-skip)
           (immediate 'undef)))

        ((and or not)
         (compile-value env `(if ,expr #t #f)))

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
         (let ((op (compile-checked env (cadr expr) (if (eq? (car expr) 'as-raw) 'val 'raw) expr)))
           (if (and (eq? (car expr) 'as-val)
                    (not (zero? (raws-in op))))
               (emit-op! 'mov 'val (list op))
               (list (car expr) op))))

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

        ((self)
         (unless (= (length expr) 1)
           (error 'compile-value "invalid self" expr))
         '(self))

        ;; (address "name"): the address of symbol name
        ((address)
         (unless (and (= (length expr) 2)
                      (string? (cadr expr)))
           (error 'compile-value "invalid address" expr))
         (cadr expr))

        (else
         (error 'compile-value "unsupported operation" expr))))

     (else
      (error 'compile-value "unsupported expression" expr))))


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


  (define (compile-args env def expr)
    (compile-operands env expr (def-params-kinds def)))


  ;; (code proc arg ...) for a call or tailcall of the non-inline def;
  ;; a closure def has no static procedure to pass, only a template
  (define (compile-direct env def expr)
    (unless (zero? (def-captures def))
      (error 'compile-direct "direct call of a closure def" expr))
    (cons* (def-code def) (def-proc def) (compile-args env def expr)))


  ;; (op dst . ops), op being call or ccall, for a def with the given result
  (define (emit-call! op result ops)
    (case result
      ((void never)
       (emit! `(,op void ,@ops))
       (if (eq? result 'never)
           (begin (emit! '(unreachable)) '(never))
           '(void)))
      (else
       (emit-op! op result ops))))


  ;; (code proc arg ...) for a call or tailcall of the procedure value (car expr)
  (define (compile-indirect env expr)
    (unless (list? expr)
      (error 'compile-indirect "invalid call" expr))
    (let* ((ops (compile-exprs env expr (map (lambda (x) 'val) expr) expr))
           (proc (car ops)))
      (compile-value (list (cons 'proc proc)) `(check-procedure proc (raw ,(length (cdr ops)))))
      (cons (emit-op! 'load 'raw (list proc '(quad 2))) ops)))


  ;; Every expression but the last for effect, then (compile-last env last)
  (define (compile-seq env exprs compile-last)
    (let-values (((init last) (split-last exprs)))
      (for-each (lambda (x) (compile-value env x)) init)
      (compile-last env last)))


  ;; The value of a def's last expression x, checked against its result
  (define (compile-result env def x)
    (let ((op (compile-value env x))
          (result (def-result def)))
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


  ;; Compiles x, whose value ends an evaluation: through if, let, let*, begin
  ;; and named lets, a call to a named let jumps back to it,
  ;; and any other expression goes to (leaf env x)
  (define (compile-exit env x leaf)
    (define (exit env x)
      (compile-exit env x leaf))

    (cond
     ((loop-call env x)
      => (lambda (loop)
           (compile-loop-call env x loop)))

     ((named-let? x)
      (compile-loop env x exit))

     ((pair? x)
      (case (car x)
        ((if)
         (let-values (((test then else) (if-parts x)))
           (let ((label-false (fresh-label)))
             (compile-branch env test #f label-false)
             (exit env then)
             (start-block! label-false)
             (exit env else))))

        ((let)
         (compile-let env x exit))

        ((let*)
         (exit env (let*->let x)))

        ((begin)
         (compile-seq env (begin-body x) exit))

        (else
         (leaf env x))))

     (else
      (leaf env x))))


  ;; Ends the function with x: a call to a non-inline def becomes a tailcall,
  ;; any other value is returned; def is the innermost def x ends, for its result
  (define (compile-tail env def x)
    (compile-exit
     env x
     (lambda (env x)
       (cond
        ((call-form? x)
         => (lambda (callee)
              (cond
               ((def-native? callee)
                (compile-return env def x))
               ((not (def-inline? callee))
                (emit! `(tailcall ,@(compile-direct env callee x))))
               ((and (eq? (def-result def) 'val) (eq? (def-result callee) 'val))
                (compile-inline env callee x
                                (lambda (env x) (compile-tail env callee x))))
               (else
                (compile-return env def x)))))

        ((indirect-call? x)
         (emit! `(tailcall ,@(compile-indirect env x))))

        (else
         (compile-return env def x))))))


  (define (named-let? x)
    (and (list? x)
         (>= (length x) 2)
         (eq? (car x) 'let)
         (symbol? (cadr x))))


  ;; The binding (loop label slot ...) when x calls a named let in env, else #f
  (define (loop-call env x)
    (and (pair? x)
         (symbol? (car x))
         (let ((entry (assq (car x) env)))
           (and entry
                (pair? (cdr entry))
                (eq? (cadr entry) 'loop)
                (cdr entry)))))


  ;; (let name ((var init) ...) body ...): each var gets its own slot, then the body
  ;; starts a block; a call (name arg ...) in exit position sets the slots and jumps back
  (define (compile-loop env expr compile-last)
    (unless (and (>= (length expr) 4)
                 (bindings? (caddr expr)))
      (error 'compile-loop "invalid named let" expr))
    (let* ((bindings (caddr expr))
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
      (compile-seq (append (map (lambda (b slot) (cons (car b) `(mutable ,slot))) bindings slots)
                           (list (cons (cadr expr) `(loop ,label ,@slots)))
                           env)
                   (cdddr expr)
                   compile-last)))


  ;; Reading a var copies its slot, so the args are computed before any slot is set
  (define (compile-loop-call env x loop)
    (let ((label (cadr loop))
          (slots (cddr loop)))
      (unless (= (length (cdr x)) (length slots))
        (error 'compile-loop-call "wrong number of arguments" x))
      (let ((ops (map-in-order (lambda (arg slot) (compile-checked env arg (kind slot) x))
                               (cdr x) slots)))
        (for-each (lambda (slot op) (emit! `(mov ,slot ,op))) slots ops)
        (emit! `(jmp ,label)))))


  ;; A named let as a value: each exit stores its value and jumps to the end;
  ;; the enclosing named lets are not in exit position, so they become (outer-loop)
  (define (compile-loop-value env expr)
    (let ((dst #f)
          (label-end (fresh-label)))
      (compile-loop (map (lambda (entry)
                           (if (and (pair? (cdr entry)) (eq? (cadr entry) 'loop))
                               (cons (car entry) '(outer-loop))
                               entry))
                         env)
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


  (define (compile-return env def x)
    (let ((op (compile-result env def x)))
      (unless (eq? (kind op) 'never)
        (emit! `(ret ,op)))))


  (define (compile-inline env def expr compile-last)
    (when (memq def (inlining))
      (error 'compile-inline "recursive inline def" (def-name def)))
    (let* ((names (def-params-names def))
           (args (compile-args env def expr))
           (callee-env (map (lambda (name arg)
                              (bind-var name arg (def-body def) #f))
                            names args)))
      (parameterize ((inlining (cons def (inlining))))
        (compile-seq callee-env (def-body def) compile-last))))


  (define (compile-let env expr compile-last)
    (unless (and (>= (length expr) 3)
                 (bindings? (cadr expr)))
      (error 'compile-let "invalid let" expr))
    (let loop ((bindings (cadr expr))
               (body-env env))
      (if (null? bindings)
          (compile-seq body-env (cddr expr) compile-last)
          (let ((op (compile-value env (cadar bindings))))
            (unless (memq (kind op) '(val raw))
              (error 'compile-let "expected a val or a raw" (car bindings)))
            (loop (cdr bindings)
                  (cons (bind-var (caar bindings) op (cddr expr) #f) body-env))))))


  ;; (let* (binding ...) body ...) => nested lets
  (define (let*->let expr)
    (unless (and (>= (length expr) 3) (list? (cadr expr)))
      (error 'let*->let "invalid let*" expr))
    (let loop ((bindings (cadr expr)))
      (if (or (null? bindings) (null? (cdr bindings)))
          `(let ,bindings ,@(cddr expr))
          `(let (,(car bindings)) ,(loop (cdr bindings))))))


  ;; (if test then else) => (values test then else)
  (define (if-parts expr)
    (unless (= (length expr) 4)
      (error 'if-parts "invalid if" expr))
    (apply values (cdr expr)))


  (define (begin-body expr)
    (unless (>= (length expr) 2)
      (error 'begin-body "invalid begin" expr))
    (cdr expr))


  ;; Whether x contains a set! of the variable name;
  ;; malformed forms are scanned as plain lists, the compiler rejects them later
  (define (assigns? name x)
    (cond
     ((not (non-empty-list? x)) #f)
     ((eq? (car x) 'quote) #f)
     ((and (eq? (car x) 'set!)
           (= (length x) 3)
           (eq? (cadr x) name))
      #t)
     (else (exists (lambda (y) (assigns? name y)) x))))


  ;; The env entry of a variable bound to op, with body its scope;
  ;; an assigned variable gets its own slot, unless op already is (own?)
  (define (bind-var name op body own?)
    (cond
     ((not (exists (lambda (x) (assigns? name x)) body))
      (cons name op))
     (own?
      (cons name `(mutable ,op)))
     (else
      (cons name `(mutable ,(emit-op! 'mov (kind op) (list op)))))))


  ;; and/or in a test: short is the truth that ends the evaluation early
  (define (compile-logic-branch env expr jump-if label)
    (assert (memq (car expr) '(and or)))
    (let ((short (eq? (car expr) 'or))
          (ops (cdr expr)))
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


  ;; Jumps to label when the truth of test equals jump-if
  (define (compile-branch env test jump-if label)
    (cond
     ((and (pair? test) (memq (car test) '(and or)))
      (compile-logic-branch env test jump-if label))

     ((and (pair? test) (eq? (car test) 'not))
      (unless (= (length test) 2)
        (error 'compile-branch "invalid not" test))
      (compile-branch env (cadr test) (not jump-if) label))

     ((test-form? test)
      => (lambda (entry)
           (let ((ops (compile-operands env test '(raw raw))))
             (emit! `(cmp ,@ops))
             (emit! `(,(if jump-if (cadr entry) (caddr entry)) ,label)))))

     ((boolean? test)
      (when (eq? test jump-if)
        (emit! `(jmp ,label))
        (start-block! (fresh-label))))

     ((and (pair? test) (eq? (car test) 'if))
      (unless (= (length test) 4)
        (error 'compile-branch "invalid if" test))
      (let ((label-else (fresh-label))
            (label-end (fresh-label)))
        (compile-branch env (cadr test) #f label-else)
        (compile-branch env (caddr test) jump-if label)
        (emit! `(jmp ,label-end))
        (start-block! label-else)
        (compile-branch env (cadddr test) jump-if label)
        (start-block! label-end)))

     ((and (pair? test) (eq? (car test) 'let) (not (named-let? test)))
      (compile-let env test (lambda (env x) (compile-branch env x jump-if label))))

     ((and (pair? test) (eq? (car test) 'let*))
      (compile-branch env (let*->let test) jump-if label))

     ((and (pair? test) (eq? (car test) 'begin))
      (compile-seq env (begin-body test)
                   (lambda (env x) (compile-branch env x jump-if label))))

     ((inline-call? test)
      => (lambda (def)
           (unless (eq? (def-result def) 'val)
             (error 'compile-branch "expected a val" test))
           (compile-inline env def test
                           (lambda (env x) (compile-branch env x jump-if label)))))

     (else
      (let ((op (compile-checked env test 'val test)))
        (emit! `(cmp ,op ,(immediate #f)))
        (emit! `(,(if jump-if 'jne 'je) ,label))))))


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


  ;; The def's entries: name, procedure object and function, or a native function;
  ;; its strings are interned, printed later with the others;
  ;; a closure def's object is the template alloc-closure copies, with its ncaptures
  (define (compile-def def)
    (let* ((params (def-params def))
           (arity (length params))
           (body (with-function
                  (lambda ()
                    (compile-seq (map (lambda (p i)
                                        (let ((arg (list 'arg (+ i 1))))
                                          (bind-var (car p)
                                                    (if (eq? (cdr p) 'raw) `(as-raw ,arg) arg)
                                                    (def-body def) #t)))
                                      params (iota arity))
                                 (def-body def)
                                 (lambda (env x) (compile-tail env def x)))))))
      (check-gc-safety (def-name def) body)
      (if (def-native? def)
          (begin
            (check-native (def-name def) body)
            `((native ,(def-code def) (arity ,arity)
                      ,@(map lower-block body))))
          (procedure-entries def arity body))))


  ;; A native def may run during a collection, so it never collects;
  ;; it has no procedure object, so no self
  (define (check-native name body)
    (define (self? x)
      (and (pair? x)
           (or (equal? x '(self))
               (self? (car x))
               (self? (cdr x)))))

    (for-each (lambda (block)
                (for-each (lambda (instr)
                            (when (or (memq (car instr) '(call tailcall alloc))
                                      (self? instr))
                              (error 'check-native "safepoint or self in a native def" name instr)))
                          (cddr block)))
              body))


  (define (procedure-entries def arity body)
    (let* ((proc (def-proc def))
           (name (and (not (def-anonymous? def)) (string-append proc "_name"))))
      `(,@(if name `((string ,name ,(symbol->string (def-as def)))) '())
        (rodata ,proc (quad ,procedure-type) (quad ,arity)
                (quad ,(def-code def)) (quad ,(or name 0)) (quad ,(def-captures def)))
        (function ,(def-code def) (arity ,arity)
                  ,@(map lower-block body)))))


  ;; Prints the IR of the program made of forms; extern defs and data are another object's.
  ;; A library (the runtime and the prelude) has no globals, exports every symbol it
  ;; defines and holds the stubs; a program exports what the library uses
  (define (compile-low forms library?)
    ;; First pass: every def is known before any code is compiled
    (let-values (((defs datas globals) (parse-program forms)))
      (parameterize ((current-defs defs)
                     (current-datas datas)
                     (current-strings '()))
        ;; Second pass: the globals and data, then the strings the defs use, then the defs
        (let* ((defs (filter (lambda (d) (and (def-emitted? d) (not (def-extern? d))))
                             (map cdr defs)))
               (datas (filter (lambda (d) (not (data-extern? d))) (map cdr datas)))
               (entries (apply append (map-in-order compile-def defs))))
          (if library?
              (print-entry `(export ,@(apply append
                                             (map (lambda (d)
                                                    (if (def-native? d)
                                                        (list (def-code d))
                                                        (list (def-code d) (def-proc d))))
                                                  defs))
                                    ,@(map data-symbol datas)))
              (begin
                (print-entry '(export "scheme_main" "scheme_globals" "scheme_nglobals"))
                (emit-globals globals)))

          (for-each (lambda (d)
                      (print-entry `(data ,(data-symbol d) ,(data-nwords d) 0)))
                    datas)

          (for-each (lambda (s)
                      (print-entry `(string ,(cdr s) ,(car s))))
                    (reverse (current-strings)))

          (for-each print-entry entries)

          (when library?
            (print-entry '(stubs))))))))
