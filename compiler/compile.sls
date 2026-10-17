;; Scheme to block IR: closure conversion, kinds, tail calls, named lets as loops, inlining.
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
  (export compile-procedure compile-low-def)
  (import (chezscheme)
          (utils)
          (parse)
          (vars)
          (defs)
          (gc-check)
          (output)
          (emit))


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
         (symbol? (car expr))
         (let ((entry (assq (car expr) env)))
           (and entry (cdr entry)))))


  (define (low-op? env expr name)
    (let ((b (head-binding env expr)))
      (and (low-op-binding? b) (eq? (low-op-binding-name b) name))))


  (define (test-form env expr)
    (let ((b (head-binding env expr)))
      (and (low-op-binding? b) (assq (low-op-binding-name b) tests))))


  ;; The def a call expr calls with the right number of arguments, or #f;
  ;; a wrong number is an error, except for a procedure, which checks it when called
  (define (called-def env expr)
    (let ((b (head-binding env expr)))
      (and (def-binding? b)
           (let ((def (def-binding-def b)))
             (cond
              ((= (length (cdr expr)) (length (def-params def))) def)
              ((def-procedure? def) #f)
              (else (error 'compile-call "wrong number of arguments" expr)))))))


  (define (immediate-operand x)
    (immediate (if (eq? x undef) 'undef x)))


  ;; Emits the code of expr and returns its operand, one of:
  ;; - a slot: (val n), (raw n), (arg n), (self)
  ;; - a constant: (quad n), or a string, the address of a symbol
  ;; - a marker: (void), (never)
  ;; - a cast: (as-val x) or (as-raw x), only for kind, removed by lower-operand
  (define (compile-value env expr)
    (cond
     ((immediate? expr)
      (immediate-operand expr))

     ((and (integer? expr) (exact? expr))
      (compile-big-integer env expr))

     ((number? expr)
      (error 'compile-value "unsupported number" expr))

     ((string? expr)
      (unless (unsafe-code?)
        (error 'compile-value "unsupported string" expr))
      (intern-string! expr))

     ((symbol? expr)
      (compile-variable env expr))

     ((vector-literal? expr)
      (compile-vector-literal env (if (vector? expr) expr (cadr expr))))

     ((expand-form env expr)
      => (lambda (x) (compile-value env x)))

     ((special-keyword env expr)
      => (lambda (keyword) (compile-special env keyword expr)))

     ((non-empty-list? expr)
      (compile-call env expr))

     (else
      (error 'compile-value "unsupported expression" expr))))


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
       ((global-binding? b)
        (call-def env (core-def 'global-ref) (list `(quad ,(global-binding-index b))) x
                  compile-result))
       ((def-binding? b)
        (def-value (def-binding-def b) x))
       ;; A data name gives the raw address of its first word
       ((data-binding? b)
        (data-symbol (data-binding-data b)))
       ((constant-binding? b)
        (constant-binding-operand b))
       ((low-op-binding? b)
        (error 'compile-value "low operation used as a value" x))
       (else
        (loop-error b x)))))


  (define (loop-error b x)
    (if (eq? (loop-binding-state b) 'lambda)
        (error 'compile-value "named let called from a lambda" x)
        (error 'compile-value "named let used as a value or called not in exit position" x)))


  (define (def-value def x)
    (unless (def-emitted? def)
      (error 'compile-value "inline def without a procedure object" x))
    (when (def-native? def)
      (error 'compile-value "native def without a procedure object" x))
    `(as-val ,(def-proc def)))


  ;; A vector literal, rebuilt at each evaluation like bignums, so it is never static;
  ;; its elements are integers, booleans, characters, () and vectors
  (define (compile-vector-literal env v)
    (let ((t (call-def env (core-def 'make-vector)
                       (list (immediate-operand (vector-length v)) (immediate-operand 0))
                       v compile-result)))
      (for-each (lambda (x i)
                  (unless (or (integer? x) (boolean? x) (char? x) (null? x) (vector? x))
                    (error 'compile-value "unsupported literal" x))
                  (call-def env (core-def 'vector-set!)
                            (list t (immediate-operand i)
                                  (compile-value env (if (null? x) ''() x)))
                            v compile-result))
                (vector->list v) (iota (vector-length v)))
      t))


  ;; The base of the chunks of a bignum literal: each chunk is a fixnum
  (define chunk-base (expt 2 30))


  ;; An integer beyond the fixnums, rebuilt by the library's + and * from chunk-base chunks,
  ;; most significant first, each with n's sign: (+ (* (+ (* c2 base) c1) base) c0)
  (define (compile-big-integer env n)
    (let loop ((m (abs n))
               (chunks '()))
      (if (= m 0)
          (fold-left (lambda (acc c)
                       (let ((product (call-def env (core-def '*)
                                                (list acc (immediate-operand chunk-base))
                                                n compile-result)))
                         (call-def env (core-def '+) (list product (immediate-operand c))
                                   n compile-result)))
                     (immediate-operand (car chunks)) (cdr chunks))
          (loop (quotient m chunk-base)
                (cons (if (< n 0) (- (remainder m chunk-base)) (remainder m chunk-base))
                      chunks)))))


  ;; The derived forms, as if, let and begin, or #f: cond, and, or, let*, and when and unless
  ;; outside unsafe defines (their body stays in tail position)
  (define (expand-form env expr)
    (case (special-keyword env expr)
      ((cond) (expand-cond env (parse-cond expr)))
      ((case) (expand-case env expr))
      ;; In a begin, since #f means no expansion
      ((and) `(,(marker 'begin) ,(expand-and (cdr expr))))
      ((or) `(,(marker 'begin) ,(expand-or (cdr expr))))
      ((let*)
       (let-values (((bindings body) (parse-let* expr)))
         (let loop ((bindings bindings))
           (if (or (null? bindings) (null? (cdr bindings)))
               `(,(marker 'let) ,bindings ,@body)
               `(,(marker 'let) (,(car bindings)) ,(loop (cdr bindings)))))))
      ;; Naive: each variable starts as #<undef> and is assigned its init in order, so assignment
      ;; conversion boxes the ones a lambda captures; letrec as letrec*
      ((letrec letrec*)
       (let-values (((bindings body) (parse-letrec expr)))
         `(,(marker 'let) ,(map (lambda (b) (list (car b) undef)) bindings)
           ,@(map (lambda (b) `(,(marker 'set!) ,@b)) bindings)
           ,@body)))
      ((when unless)
       (and (not (unsafe-code?))
            (begin
              (unless (>= (length expr) 3)
                (error 'expand "invalid when or unless" expr))
              (let ((body `(,(marker 'begin) ,@(cddr expr))))
                (if (eq? (car expr) 'when)
                    `(,(marker 'if) ,(cadr expr) ,body ,undef)
                    `(,(marker 'if) ,(cadr expr) ,undef ,body))))))
      (else #f)))


  ;; Each clause is (test expr ...), (test), (test => f), or a last (else expr ...)
  (define (expand-cond env clauses)
    (if (null? clauses)
        undef
        (let ((test (caar clauses))
              (body (cdar clauses))
              (rest (cdr clauses)))
          (cond
           ((auxiliary? env test 'else)
            (unless (and (null? rest) (pair? body))
              (error 'expand-cond "invalid else clause" (car clauses)))
            `(,(marker 'begin) ,@body))
           ((null? body)
            (let ((t (gensym "cond")))
              `(,(marker 'let) ((,t ,test))
                (,(marker 'if) ,t ,t ,(expand-cond env rest)))))
           ((auxiliary? env (car body) '=>)
            (unless (= (length body) 2)
              (error 'expand-cond "invalid => clause" (car clauses)))
            (let ((t (gensym "cond")))
              `(,(marker 'let) ((,t ,test))
                (,(marker 'if) ,t (,(cadr body) ,t) ,(expand-cond env rest)))))
           (else
            `(,(marker 'if) ,test (,(marker 'begin) ,@body) ,(expand-cond env rest)))))))


  ;; Each clause is ((datum ...) expr ...), ((datum ...) => f), or a last (else expr ...) or
  ;; (else => f); the key is compared with eq?, so the datums are immediates
  (define (expand-case env expr)
    (let-values (((key clauses) (parse-case expr)))
      (let ((t (gensym "case")))
        (define (body clause)
          (if (auxiliary? env (cadr clause) '=>)
              (begin
                (unless (= (length clause) 3)
                  (error 'expand-case "invalid => clause" clause))
                `(,(caddr clause) ,t))
              `(,(marker 'begin) ,@(cdr clause))))
        (define (datum x)
          (unless (or (immediate? x) (null? x))
            (error 'expand-case "unsupported case datum" x))
          (if (null? x) ''() x))
        `(,(marker 'let) ((,t ,key))
          ,(let loop ((clauses clauses))
             (cond
              ((null? clauses) undef)
              ((auxiliary? env (caar clauses) 'else)
               (unless (null? (cdr clauses))
                 (error 'expand-case "invalid else clause" (car clauses)))
               (body (car clauses)))
              (else
               (unless (list? (caar clauses))
                 (error 'expand-case "invalid case clause" (car clauses)))
               `(,(marker 'if)
                 (,(marker 'or) ,@(map (lambda (d) `(,(marker 'eq?) ,t ,(datum d))) (caar clauses)))
                 ,(body (car clauses))
                 ,(loop (cdr clauses))))))))))


  ;; else and => are keywords unless bound
  (define (auxiliary? env x keyword)
    (and (eq? x keyword)
         (not (assq keyword env))))


  ;; The value of the deciding operand
  (define (expand-and exprs)
    (cond
     ((null? exprs) #t)
     ((null? (cdr exprs)) (car exprs))
     (else `(,(marker 'if) ,(car exprs) ,(expand-and (cdr exprs)) #f))))


  (define (expand-or exprs)
    (cond
     ((null? exprs) #f)
     ((null? (cdr exprs)) (car exprs))
     (else
      (let ((t (gensym "or")))
        `(,(marker 'let) ((,t ,(car exprs)))
          (,(marker 'if) ,t ,t ,(expand-or (cdr exprs))))))))


  ;; if, let, begin, lambda, set!, and when and unless in unsafe defines
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

      ((lambda)
       (let-values (((args body) (parse-lambda expr)))
         (compile-lambda env #f args body)))

      ((set!)
       (let-values (((var value) (parse-set! expr)))
         (compile-set! env var value)))

      ;; The body's value when it is a val, else undef, so the body may have any kind
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
       ((low-op-binding? b)
        (compile-low-op env (low-op-binding-name b) expr))
       ((loop-binding? b)
        (loop-error b expr))
       ((called-def env expr)
        => (lambda (def)
             (call-def env def (compile-args env def expr) expr compile-result)))
       (else
        (emit-op! 'call 'val (compile-indirect env expr))))))


  ;; Calls def with the operands ops, compiling an inline def's last expression with
  ;; (compile-last env def x)
  (define (call-def env def ops form compile-last)
    (cond
     ((def-inline? def)
      (compile-inline def ops form compile-last))
     ;; A System V call, so not a safepoint
     ((def-native? def)
      (emit-call! 'ccall (def-result def) (cons (def-code def) ops)))
     (else
      (emit-call! 'call (def-result def) (cons* (def-code def) (def-proc def) ops)))))


  (define (compile-low-op env op expr)
    (cond
     ((assq op tests)
      (compile-if env expr #t #f))

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


  (define (compile-args env def expr)
    (compile-operands env expr (def-params-kinds def)))


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
    (let* ((ops (compile-exprs env expr (map (lambda (x) 'val) expr) expr))
           (proc (car ops)))
      (call-def env (core-def 'check-procedure) (list proc `(quad ,(length (cdr ops)))) expr
                compile-result)
      (cons (emit-op! 'load 'raw (list proc '(quad 2))) ops)))


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
        (case (special-keyword env x)
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
               (special? env x)
               (immediate? x)
               (vector-literal? x)
               (low-op-binding? b))
           (compile-return env def x))

          ((called-def env x)
           => (lambda (callee)
                (cond
                 ((def-native? callee)
                  (compile-return env def x))
                 ((not (def-inline? callee))
                  (emit! `(tailcall ,(def-code callee) ,(def-proc callee)
                                    ,@(compile-args env callee x))))
                 ((and (eq? (def-result def) 'val) (eq? (def-result callee) 'val))
                  (compile-inline callee (compile-args env callee x) x
                                  (lambda (env callee x) (compile-tail env callee x))))
                 (else
                  (compile-return env def x)))))

          (else
           (emit! `(tailcall ,@(compile-indirect env x)))))))))


  (define (compile-return env def x)
    (let ((op (compile-result env def x)))
      (unless (eq? (kind op) 'never)
        (emit! `(ret ,op)))))


  ;; (let name ((var init) ...) body ...): each var gets its own slot, then the body
  ;; starts a block; a call (name arg ...) in exit position sets the slots and jumps back
  (define (compile-loop env expr compile-last)
    (let-values (((name bindings body) (parse-named-let expr)))
      (let* ((vars (map car bindings))
             (free (free-vars (bind-scope env (cons name vars)) body))
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
                    (when (boxed? v free)
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
    (let* ((names (map car (def-params def)))
           (env (def-env def))
           (vars (free-vars (bind-scope env names) (def-body def)))
           (callee-env (append (map (lambda (name op) (bind-local name op vars #f)) names ops)
                               env)))
      (parameterize ((inlining (cons def (inlining)))
                     (unsafe-code? (def-unsafe? def)))
        (compile-seq callee-env (def-body def)
                     (lambda (env x) (compile-last env def x))))))


  ;; The env entry of the variable name bound to op, vars being the variables of its scope:
  ;; a box when it is assigned and captured, its own slot when it is only assigned (op itself
  ;; when own?)
  (define (bind-local name op vars own?)
    (cond
     ((boxed? name vars)
      (when (eq? (kind op) 'raw)
        (error 'bind-local "captured raw variable" name))
      (cons name (make-local-binding (call-def '() (core-def 'make-box) (list op) name compile-result)
                                     #t)))
     ((not (assigned? name vars))
      (cons name (make-local-binding op #f)))
     (own?
      (cons name (make-local-binding `(mutable ,op) #f)))
     (else
      (cons name (make-local-binding `(mutable ,(emit-op! 'mov (kind op) (list op))) #f)))))


  (define (compile-let env expr compile-last)
    (let-values (((bindings body) (parse-let expr)))
      (let* ((vars (free-vars (bind-scope env (map car bindings)) body))
             (ops (map-in-order (lambda (b)
                                  (let ((op (compile-named env (car b) (cadr b))))
                                    (unless (memq (kind op) '(val raw))
                                      (error 'compile-let "expected a val or a raw" b))
                                    op))
                                bindings)))
        (compile-seq (append (map (lambda (b op) (bind-local (car b) op vars #f)) bindings ops)
                             env)
                     body compile-last))))


  ;; expr, naming it name if it is a lambda
  (define (compile-named env name expr)
    (if (eq? (special-keyword env expr) 'lambda)
        (let-values (((args body) (parse-lambda expr)))
          (compile-lambda env name args body))
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
       ((global-binding? b)
        (call-def env (core-def 'global-set!) (list `(quad ,(global-binding-index b)) src) var
                  compile-result))
       (else
        (error 'compile-set! "set! of a library procedure or primitive" var)))
      (immediate 'undef)))


  ;; Lifts the lambda into its own function, and returns its procedure object, or a closure
  ;; filled with the captures
  (define (compile-lambda env name args body)
    (let* ((captures (map (lambda (var) (assq var env))
                          (reverse (captured-vars env args body))))
           (def (make-def (or name 'lambda) (mangle (fresh-name (or name 'lambda)))
                          (map (lambda (x) (cons x 'val)) args) 'val
                          #f #f #f #t (unsafe-code?) (length captures) (not name) #f body #f)))
      (for-each (lambda (c)
                  (when (eq? (kind (local-binding-operand (cdr c))) 'raw)
                    (error 'compile-lambda "captured raw variable" (car c))))
                captures)
      (add-def-symbols! def)
      (compile-procedure def env captures)
      (if (null? captures)
          `(as-val ,(def-proc def))
          (let ((c (call-def env (core-def 'alloc-closure) (list (def-proc def)) name
                             compile-result)))
            (for-each (lambda (capture i)
                        (let* ((b (cdr capture))
                               (op (local-binding-operand b)))
                          (call-def env (core-def 'closure-set!)
                                    (list c `(quad ,i)
                                          (if (mutable? op) (emit-op! 'mov 'val (cdr op)) op))
                                    name compile-result)))
                      captures (iota (length captures)))
            (emit-op! 'mov 'val (list `(as-val ,c)))))))


  ;; Compiles the procedure def (a lambda, a procedure define or main) in env, the enclosing
  ;; env, with its captures, (name . binding) pairs of env, loaded from the closure
  (define (compile-procedure def env captures)
    (let* ((args (map car (def-params def)))
           (body (def-body def))
           (outer (disable-loops (filter (lambda (b) (not (local-binding? (cdr b)))) env) 'lambda))
           (blocks
            (with-function
             (lambda ()
               (parameterize ((unsafe-code? (def-unsafe? def)))
                 (let* ((inner (append (map (lambda (c i)
                                              (cons (car c)
                                                    (make-local-binding
                                                     (call-def env (core-def 'closure-ref)
                                                               (list `(quad ,i)) (car c)
                                                               compile-result)
                                                     (local-binding-box? (cdr c)))))
                                            captures (iota (length captures)))
                                       outer))
                        (vars (free-vars (bind-scope inner args) body))
                        (body-env (append (map (lambda (x i) (bind-local x `(arg ,(+ i 1)) vars #t))
                                               args (iota (length args)))
                                          inner)))
                   (compile-seq body-env body (lambda (env x) (compile-tail env def x)))))))))
      (check-gc-safety (def-name def) blocks)
      (add-entries! (procedure-entries def blocks))))


  ;; A def with typed parameters, compiled in its env
  (define (compile-low-def def)
    (let* ((params (def-params def))
           (env (def-env def))
           (blocks
            (with-function
             (lambda ()
               (parameterize ((unsafe-code? (def-unsafe? def)))
                 (let* ((vars (free-vars (bind-scope env (map car params)) (def-body def)))
                        (body-env (append (map (lambda (p i)
                                                 (when (boxed? (car p) vars)
                                                   (error 'compile-low-def
                                                          "assigned and captured parameter" (car p)))
                                                 (let ((arg `(arg ,(+ i 1))))
                                                   (bind-local (car p)
                                                               (if (eq? (cdr p) 'raw) `(as-raw ,arg) arg)
                                                               vars #t)))
                                               params (iota (length params)))
                                          env)))
                   (compile-seq body-env (def-body def)
                                (lambda (env x) (compile-tail env def x)))))))))
      (check-gc-safety (def-name def) blocks)
      (if (def-native? def)
          (begin
            (check-native (def-name def) blocks)
            (add-entries! `((native ,(def-code def) (arity ,(length params))
                                    ,@(map lower-block blocks)))))
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
    (let ((keyword (special-keyword env test)))
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

       ((test-form env test)
        => (lambda (entry)
             (let ((ops (compile-operands env test '(raw raw))))
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
                 (compare-false (call-def env def (compile-args env def test) test compile-result)
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


  ;; The def's name, procedure object and function; a closure def's object is the template
  ;; alloc-closure copies, with its ncaptures
  (define (procedure-entries def blocks)
    (let* ((proc (def-proc def))
           (arity (length (def-params def)))
           (name (and (not (def-anonymous? def)) (string-append proc "_name"))))
      `(,@(if name `((string ,name ,(symbol->string (def-name def)))) '())
        (rodata ,proc (quad ,procedure-type) (quad ,arity)
                (quad ,(def-code def)) (quad ,(or name 0)) (quad ,(def-captures def)))
        (function ,(def-code def) (arity ,arity)
                  ,@(map lower-block blocks))))))
