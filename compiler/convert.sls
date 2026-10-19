;; The core AST to CPS, one fun per function, then lowered to block IR (lower.sls).
;;
;; An expression is converted either
;; - as a value: (cps env x c), c a Scheme procedure given x's atom and returning the term of
;;   the rest of the computation (a meta-continuation, so no administrative continuation is
;;   made); an atom of kind void or never is (void) or (never), and the term after a never is
;;   dropped, c being called only for its errors
;; - in exit position: (cps-exit env x leaf), through if, let, begin, named lets and fix, a
;;   call to a named let throwing to its continuation
;; - in tail position: (cps-tail env def x), an exit where a call becomes a tail call, any other
;;   value is returned
;; - as a test: (cps-test env x kt kf), branching to the continuation kt or kf
;; Calls allocate the callee's frame first and store each argument in it as soon as it is
;; computed, as the block IR wants them; inline defs are expanded.

(library (convert)
  (export compile-procedure compile-low-def compile-literal-init)
  (import (chezscheme)
          (utils)
          (parse)
          (vars)
          (closure)
          (defs)
          (ops)
          (output)
          (compile)
          (cps)
          (lower))


  ;; Per function: its def, for self tail calls, the kinds of its variables, its return
  ;; continuation, and the counter numbering its names
  (define current-function (make-parameter #f))
  (define kinds (make-parameter #f))
  (define return-k (make-parameter #f))
  (define name-counter (make-parameter #f))


  ;; A name numbered in its function, for the dumps
  (define (new-name prefix)
    (gensym (string-append prefix (number->string ((name-counter))))))


  ;; Compiles the procedure def (a lambda, a procedure define or main) in env, the enclosing
  ;; env, of which it sees the static procedures; its body binds its captures. main first calls
  ;; the startup defs.
  (define (compile-procedure def env)
    (let* ((args (def-core-names def))
           (outer (disable-loops (filter (lambda (b) (not (local-binding? (cdr b)))) env) 'lambda))
           (env (append (map (lambda (x i) (cons x (make-local-binding `(arg ,(+ i 1)))))
                             args (iota (length args)))
                        outer)))
      (convert-function
       def
       (lambda ()
         (let loop ((defs (if (string=? (def-symbol def) "main") (startup-defs) '())))
           (if (pair? defs)
               (call-def (car defs) '() 'main (lambda (ignored) (loop (cdr defs))))
               (cps-seq env (def-core-body def) (lambda (env x) (cps-tail env def x)))))))))


  ;; A def with typed parameters
  (define (compile-low-def def)
    (let ((env (map (lambda (name k i)
                      (let ((arg `(arg ,(+ i 1))))
                        (cons name (make-local-binding (if (eq? k 'raw) `(as-raw ,arg) arg)))))
                    (def-core-names def) (def-params-kinds def) (iota (length (def-params def))))))
      (convert-function
       def
       (lambda ()
         (cps-seq env (def-core-body def) (lambda (env x) (cps-tail env def x)))))))


  ;; The function of def, which fills the literal cells of the compilation at startup, in order:
  ;; a string's with literal-string of its UTF-8 bytes, a static string, a symbol's with
  ;; string->symbol of its name's cell, filled before
  (define (compile-literal-init def)
    (convert-function
     def
     (lambda ()
       (parameterize ((unsafe-code? #t))
         (let loop ((literals (reverse (current-literals))))
           (if (null? literals)
               (cps-tail '() def undef)
               (let* ((x (caar literals))
                      (store (lambda (a)
                               `(letprim #f (store ,(literals-symbol) (quad ,(cdar literals)) ,a)
                                         ,(loop (cdr literals))))))
                 (if (symbol? x)
                     (letprim `(literal ,(symbol->string x)) 'val
                              (lambda (name)
                                (call-def (core-def 'string->symbol) (list name) x store)))
                     (call-def (core-def 'literal-string)
                               (list (intern-string! x)
                                     `(quad ,(bytevector-length (string->utf8 x)))
                                     `(quad ,(string-length x))
                                     `(quad ,(if (ascii? x) 1 0)))
                               x store)))))))))


  (define (ascii? s)
    (for-all (lambda (c) (< (char->integer c) 128)) (string->list s)))


  ;; The function of def, its body the term (make-term), converted, checked and lowered
  (define (convert-function def make-term)
    (emit-function!
     def
     (lambda ()
       (parameterize ((unsafe-code? (def-unsafe? def))
                      (current-function def)
                      (kinds (make-eq-hashtable))
                      (name-counter (make-counter 1))
                      (return-k (gensym "return")))
         (let ((fun `(fun ,(def-code def) (,(return-k)) ,(make-term))))
           (dumped 'cps (list-head fun 3) (cdddr fun))
           (check-cps fun)
           (lower-function! def fun))))))


  ;; The enclosing named lets, as state (outer or lambda)
  (define (disable-loops env state)
    (map (lambda (entry)
           (if (and (loop-binding? (cdr entry)) (eq? (loop-binding-state (cdr entry)) 'here))
               (cons (car entry) (make-loop-binding #f '() state))
               entry))
         env))


  ;; A def reached by a Scheme call, with a frame, rather than inlined or a noalloc call
  (define (def-called? def)
    (not (or (def-inline? def) (def-noalloc? def))))


  (define (fresh kind)
    (let ((x (new-name "t")))
      (hashtable-set! (kinds) x kind)
      x))


  (define (atom-kind a)
    (if (symbol? a)
        (or (hashtable-ref (kinds) a #f) (error 'convert "variable without a kind" a))
        (kind a)))


  ;; As check-kind
  (define (check-atom a k form)
    (let ((ak (atom-kind a)))
      (unless (if (eq? k 'any) (memq ak '(val raw)) (eq? ak k))
        (error 'check-kind (if (eq? k 'any) "expected a val or a raw" (format "expected a ~a" k))
               form))))


  (define (param x)
    (if (eq? (atom-kind x) 'raw) `(raw ,x) x))


  ;; (letprim x op (c x)) for a result of kind k
  (define (letprim op k c)
    (case k
      ((void) `(letprim #f ,op ,(c '(void))))
      ((never) `(letprim #f ,op ,(never c)))
      (else (let* ((x (fresh k))
                   (body (c x)))
              `(letprim ,x ,op ,body)))))


  ;; The end of a computation that does not return: c only for its errors
  (define (never c)
    (c '(never))
    '(unreachable))


  ;; The throw of the atom a to j, which has a parameter only for a val or a raw
  (define (deliver j a)
    (case (atom-kind a)
      ((never) '(unreachable))
      ((void) `(throw ,j))
      (else `(throw ,j ,a))))


  ;; c called with the parameters of the enclosing inline expansions, for an atom given from
  ;; inside a later one
  (define (outside c)
    (let ((i (inlining))
          (u (unsafe-code?)))
      (lambda (a)
        (parameterize ((inlining i) (unsafe-code? u))
          (c a)))))


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


  ;; The low test expr's name, or #f
  (define (test-form expr)
    (and (eq? (special-keyword expr) 'low) (low-test (cadr expr)) (cadr expr)))


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


  (define (cps env x c)
    (cond
     ((immediate? x)
      (c (immediate-operand x)))

     ((exact-rational? x)
      (c `(as-val ,(intern-number! x))))

     ((number? x)
      (error 'compile-value "unsupported number" x))

     ;; Outside unsafe code, a Scheme string, loaded from its literal cell
     ((string? x)
      (if (unsafe-code?)
          (c (intern-string! x))
          (letprim `(literal ,x) 'val c)))

     ((quoted-symbol? x)
      (letprim `(literal ,(quoted-symbol-name x)) 'val c))

     ((symbol? x)
      (cps-variable env x c))

     ((def? x)
      (c (def-value x (def-name x))))

     ;; A data name gives the raw address of its first word
     ((data? x)
      (c (data-symbol x)))

     ((vector-literal? x)
      (cps-vector-literal env (if (vector? x) x (cadr x)) c))

     ((expand-form x)
      => (lambda (y) (cps env y c)))

     ((special-keyword x)
      => (lambda (keyword) (cps-special env keyword x c)))

     ((non-empty-list? x)
      (cps-call env x c))

     (else
      (error 'compile-value "unsupported expression" x))))


  (define (cps-variable env x c)
    (let ((b (lookup env x)))
      (cond
       ((local-binding? b)
        (c (local-binding-operand b)))
       ;; A static procedure of a fix
       ((def-binding? b)
        (c (def-value (def-binding-def b) x)))
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
  (define (cps-vector-literal env v c)
    (call-def (core-def 'make-vector)
              (list (immediate-operand (vector-length v)) (immediate-operand 0))
              v
              (lambda (t)
                (let loop ((xs (vector->list v)) (i 0))
                  (if (null? xs)
                      (c t)
                      (let ((x (car xs)))
                        (unless (or (exact-rational? x) (boolean? x) (char? x) (string? x)
                                    (symbol? x) (null? x) (vector? x))
                          (error 'compile-value "unsupported literal" x))
                        (cps env (cond
                                  ((null? x) ''())
                                  ((symbol? x) (make-quoted-symbol x))
                                  (else x))
                             (lambda (a)
                               (call-def (core-def 'vector-set!) (list t (immediate-operand i) a) v
                                         (lambda (ignored) (loop (cdr xs) (+ i 1))))))))))))


  ;; and and or as values, as if, let and begin, or #f
  (define (expand-form expr)
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
  (define (cps-special env keyword x c)
    (case keyword
      ((if)
       (let-values (((test then else) (parse-if x)))
         (cps-if env test then else c)))

      ((let)
       (if (named-let? x)
           (cps-loop-value env x c)
           (cps-let env x (lambda (env y) (cps env y c)))))

      ((begin)
       (cps-seq env (parse-begin x) (lambda (env y) (cps env y c))))

      ((closure)
       (cps-lambda env #f x c))

      ;; A cell is a slot, its reads copied, so a later %cell-set! does not change an atom
      ;; already computed
      ((%cell)
       (cps env (cadr x)
            (lambda (a)
              (unless (memq (atom-kind a) '(val raw))
                (error 'compile-value "expected a val or a raw" x))
              (letprim `(move ,a) (atom-kind a) c))))

      ((%cell-ref)
       (cps env (cadr x)
            (lambda (a)
              (letprim `(move ,a) (atom-kind a) c))))

      ((%cell-set!)
       (cps env (cadr x)
            (lambda (cell)
              (cps env (caddr x)
                   (lambda (a)
                     (check-atom a (atom-kind cell) (cadr x))
                     `(letprim #f (cell-set! ,cell ,a) ,(c '(void))))))))

      ((fix)
       (cps-fix env x (lambda (env y) (cps env y c))))

      ((check-defined)
       (cps-check-defined env x c))

      ((const)
       (c (cadr x)))

      ((low)
       (cps-low env (cadr x) (cdr x) c))

      ;; Only in unsafe defines: the body's value when it is a val, else undef, so the body may
      ;; have any kind
      ((when unless)
       (unless (>= (length x) 3)
         (error 'compile-value "invalid when or unless" x))
       (let* ((j (new-name "j"))
              (k #f)
              (body (cps-seq env (cddr x)
                             (lambda (env y)
                               (cps env y
                                    (lambda (a)
                                      (set! k (atom-kind a))
                                      (case k
                                        ((val) `(throw ,j ,a))
                                        ((never) '(unreachable))
                                        (else `(throw ,j))))))))
              (val? (eq? k 'val))
              (skip (if val? (new-name "k") j))
              (bodyk (new-name "k"))
              (test (if (eq? keyword 'when)
                        (cps-test env (cadr x) bodyk skip)
                        (cps-test env (cadr x) skip bodyk)))
              (r (and val? (fresh 'val)))
              (rest (c (or r (immediate 'undef))))
              (inner `(letcont ((,bodyk () ,body)) ,test)))
         `(letcont ((,j ,(if r (list r) '()) ,rest))
                   ,(if val?
                        `(letcont ((,skip () (throw ,j ,(immediate 'undef)))) ,inner)
                        inner))))

      (else
       (error 'compile-value "unsupported form" x))))


  (define (cps-call env x c)
    (let ((b (head-binding env x)))
      (cond
       ((loop-binding? b)
        (loop-error b x))
       ((called-def env x)
        => (lambda (def)
             (if (def-called? def)
                 (cps-direct env def x #f c)
                 (cps-args env def x (lambda (as) (call-def def as x c))))))
       (else
        (cps-indirect env x #f c)))))


  ;; Calls def with the atoms as, giving its value to c
  (define (call-def def as form c)
    (cond
     ((def-inline? def)
      (let ((c (outside c)))
        (cps-inline def as form (lambda (env def x) (cps-result env def x c)))))
     ;; A call on the machine stack, so not a safepoint
     ((def-noalloc? def)
      (letprim `(stackcall ,(def-result def) ,(def-code def) ,@as) (def-result def) c))
     (else
      (direct-frame def form
                    (lambda (f)
                      (frame-stores f as
                                    (lambda ()
                                      (call-to (def-code def) f (def-result def) c))))))))


  ;; The non-tail call of target with the frame f, its value, of kind result, given to c
  (define (call-to target f result c)
    (if (eq? result 'never)
        (begin
          (c '(never))
          `(call never ,target ,f))
        (let* ((k (new-name "k"))
               (r (fresh result))
               (body (c r)))
          `(letcont ((,k (,(param r)) ,body))
                    (call ,k ,target ,f)))))


  (define (call-def-tail def as form)
    (direct-frame def form
                  (lambda (f)
                    (frame-stores f as (lambda () `(call ,(return-k) ,(def-code def) ,f))))))


  ;; The frame of a call to def, filled before the call
  (define (direct-frame def form c)
    (call-def (core-def 'make-direct-frame) (list (def-code def) `(as-val ,(def-proc def))) form c))


  (define (frame-store f i a k)
    `(letprim #f (store (as-raw ,f) (quad ,(+ i 6)) ,a) ,(k)))


  (define (frame-stores f as k)
    (let loop ((as as) (i 0))
      (if (null? as)
          (k)
          (frame-store f i (car as) (lambda () (loop (cdr as) (+ i 1)))))))


  ;; Each argument stored as soon as it is computed, so only the frame is live across the
  ;; calls they make
  (define (cps-frame-args env f exprs form k)
    (let loop ((exprs exprs) (i 0))
      (if (null? exprs)
          (k)
          (cps-checked env (car exprs) 'val form
                       (lambda (a)
                         (frame-store f i a (lambda () (loop (cdr exprs) (+ i 1)))))))))


  ;; The call (def arg ...) of a def-called? def, a tail call when tail?
  (define (cps-direct env def x tail? c)
    (direct-frame
     def x
     (lambda (f)
       (let ((call (lambda ()
                     (if tail?
                         `(call ,(return-k) ,(def-code def) ,f)
                         (call-to (def-code def) f (def-result def) c)))))
         (if (def-rest? def)
             (let ((r (def-required def)))
               (cps-frame-args env f (list-head (cdr x) r) x
                               (lambda ()
                                 (cps-rest-list env (list-tail (cdr x) r) x
                                                (lambda (l) (frame-store f r l call))))))
             (cps-frame-args env f (cdr x) x call))))))


  ;; x is (name arg ...), converted after its entry in the table of ops.sls
  (define (cps-low env name x c)
    (let ((op (low-op name)))
      (define (literal? ok?)
        (and (= (length x) 2) (ok? (cadr x))))

      (cond
       ((op-test? op)
        (cps-if env (cons (marker 'low) x) #t #f c))

       (else
        (case name
          ((raw)
           (unless (literal? (lambda (n)
                               (and (integer? n) (exact? n) (<= (- (expt 2 63)) n (- (expt 2 64) 1)))))
             (error 'compile-value "invalid raw literal" x))
           (c `(quad ,(cadr x))))

          ;; Casts only wrap the atom, except that as-val copies a raw slot into a tmp, since
          ;; the GC scans only tmps
          ((as-raw)
           (cps-operands env x (op-operands op) (lambda (as) (c `(as-raw ,(car as))))))

          ((as-val)
           (cps-operands env x (op-operands op) (lambda (as) (letprim `(as-val ,(car as)) 'val c))))

          ((syscall)
           (unless (and (<= 2 (length x) 8)
                        (integer? (cadr x))
                        (exact? (cadr x))
                        (>= (cadr x) 0))
             (error 'compile-value "invalid syscall" x))
           (cps-exprs env (cddr x) (map (lambda (y) 'raw) (cddr x)) x
                      (lambda (as) (letprim `(syscall (quad ,(cadr x)) ,@as) 'raw c))))

          ((alloc)
           (cps-operands env x (op-operands op)
                         (lambda (as)
                           (let ((n (car as)))
                             (when (and (pair? n) (eq? (car n) 'quad) (<= (cadr n) 0))
                               (error 'compile-value "invalid alloc" x)))
                           (letprim `(alloc ,@as) 'raw c))))

          ((tail-call-frame)
           (cps-operands env x (op-operands op)
                         (lambda (as)
                           (c '(never))
                           `(call ,(return-k) frame ,(car as)))))

          ((self)
           (cps-operands env x (op-operands op) (lambda (as) (c '(self)))))

          ((address)
           (unless (literal? string?)
             (error 'compile-value "invalid address" x))
           (c (cadr x)))

          (else
           (cps-operands env x (op-operands op)
                         (lambda (as) (letprim `(,name ,@as) (op-result op) c)))))))))


  (define (cps-checked env x k form c)
    (cps env x
         (lambda (a)
           (check-atom a k form)
           (c a))))


  (define (cps-operands env x kinds c)
    (cps-exprs env (cdr x) kinds x c))


  ;; form is the whole expression, for errors
  (define (cps-exprs env xs kinds form c)
    (unless (= (length xs) (length kinds))
      (error 'compile-exprs "wrong number of operands" form))
    (let loop ((xs xs) (kinds kinds) (as '()))
      (if (null? xs)
          (c (reverse as))
          (cps-checked env (car xs) (car kinds) form
                       (lambda (a) (loop (cdr xs) (cdr kinds) (cons a as)))))))


  ;; The atoms of a call to def, the extra ones packed into its rest parameter's list
  (define (cps-args env def x c)
    (if (def-rest? def)
        (let ((required (list-head (cdr x) (def-required def))))
          (cps-exprs env required (map (lambda (y) 'val) required) x
                     (lambda (as)
                       (cps-rest-list env (list-tail (cdr x) (def-required def)) x
                                      (lambda (l) (c (append as (list l))))))))
        (cps-exprs env (cdr x) (def-params-kinds def) x c)))


  ;; A fresh list of the values of xs, consed from the last
  (define (cps-rest-list env xs form c)
    (cps-exprs env xs (map (lambda (y) 'val) xs) form
               (lambda (as)
                 (let loop ((as (reverse as)) (acc (immediate-operand ''())))
                   (if (null? as)
                       (c acc)
                       (call-def (core-def 'cons) (list (car as) acc) form
                                 (lambda (p) (loop (cdr as) p))))))))


  ;; The call of the procedure value (car x), a tail call when tail?; a tail call that may be a
  ;; self call computes its arguments first, so that it can reuse the frame, any other call
  ;; allocates the frame first
  (define (cps-indirect env x tail? c)
    (let ((n (length (cdr x)))
          (self (current-function)))
      (cps-checked
       env (car x) 'val x
       (lambda (proc)
         (if (and tail? (not (def-rest? self)) (= n (length (def-params self))))
             (cps-exprs env (cdr x) (map (lambda (y) 'val) (cdr x)) x
                        (lambda (as)
                          (let* ((other (new-name "k"))
                                 (same (new-name "k"))
                                 (test (cps-inline (core-def 'same-code?) (list proc (def-code self)) x
                                                   (test-last same other)))
                                 (call (frame-call proc n (lambda (f k) (frame-stores f as k)) x #t #f)))
                            `(letcont ((,other () ,call))
                                      (letcont ((,same () (selfcall ,proc ,@as)))
                                               ,test)))))
             (frame-call proc n (lambda (f k) (cps-frame-args env f (cdr x) x k)) x tail? c))))))


  ;; The inline def's last expression as a test
  (define (test-last kt kf)
    (lambda (env def x)
      (cps-test env x kt kf)))


  ;; The call of proc with n arguments, (fill f k) storing them in its frame f before the term
  ;; (k); when the arity is not n, the frame is a placeholder holding them, for apply-frame
  (define (frame-call proc n fill form tail? c)
    (call-def
     (core-def 'make-call-frame) (list proc `(quad ,n)) form
     (lambda (f)
       (fill f
             (lambda ()
               (let* ((fast (new-name "k"))
                      (slow (new-name "k"))
                      (test (cps-inline (core-def 'frame-fits?) (list f `(quad ,n)) form
                                        (test-last fast slow))))
                 (if tail?
                     (let ((slow-term (call-def-tail (core-def 'apply-frame) (list f) form)))
                       `(letcont ((,slow () ,slow-term))
                                 (letcont ((,fast () (call ,(return-k) frame ,f)))
                                          ,test)))
                     (let* ((join (new-name "j"))
                            (slow-term (call-def (core-def 'apply-frame) (list f) form
                                                 (lambda (a) `(throw ,join ,a))))
                            (r (fresh 'val))
                            (rest (c r)))
                       `(letcont ((,join (,r) ,rest))
                                 (letcont ((,slow () ,slow-term))
                                          (letcont ((,fast () (call ,join frame ,f)))
                                                   ,test)))))))))))


  ;; Every expression but the last for effect, then (last env last)
  (define (cps-seq env exprs last)
    (let loop ((exprs exprs))
      (if (null? (cdr exprs))
          (last env (car exprs))
          (cps env (car exprs) (lambda (a) (loop (cdr exprs)))))))


  ;; The value of a def's last expression x, checked against its result
  (define (cps-result env def x c)
    (cps env x (lambda (a) (finish def a c))))


  (define (finish def a c)
    (let ((result (def-result def)))
      (cond
       ((eq? (atom-kind a) 'never)
        (c a))
       ((eq? result 'never)
        (never c))
       ((eq? result 'void)
        (c '(void)))
       (else
        (check-atom a result (def-name def))
        (c a)))))


  ;; x, whose value ends an evaluation: through if, let, begin, named lets and fix, a call to a
  ;; named let throws to it, and any other expression goes to (leaf env x)
  (define (cps-exit env x leaf)
    (define (exit env x)
      (cps-exit env x leaf))

    (let ((b (head-binding env x)))
      (cond
       ((loop-binding? b)
        (unless (eq? (loop-binding-state b) 'here)
          (loop-error b x))
        (cps-loop-call env x b))

       ((expand-form x)
        => (lambda (y) (exit env y)))

       (else
        (case (special-keyword x)
          ((if)
           (let-values (((test then else) (parse-if x)))
             (let* ((kt (new-name "k"))
                    (kf (new-name "k"))
                    (test (cps-test env test kt kf))
                    (then (exit env then))
                    (else (exit env else)))
               `(letcont ((,kf () ,else))
                         (letcont ((,kt () ,then))
                                  ,test)))))

          ((let)
           (if (named-let? x)
               (cps-loop env x exit)
               (cps-let env x exit)))

          ((begin)
           (cps-seq env (parse-begin x) exit))

          ((fix)
           (cps-fix env x exit))

          (else
           (leaf env x)))))))


  ;; Ends the function with x: a call to a non-inline def becomes a tail call, any other value
  ;; is returned; def is the innermost def x ends, for its result
  (define (cps-tail env def x)
    (cps-exit
     env x
     (lambda (env x)
       (cond
        ((or (not (non-empty-list? x))
             (special? x)
             (immediate? x)
             (vector-literal? x))
         (cps-return env def x))

        ((called-def env x)
         => (lambda (callee)
              (cond
               ((def-noalloc? callee)
                (cps-return env def x))
               ((eq? callee (current-function))
                (cps-args env callee x (lambda (as) `(selfcall (self) ,@as))))
               ((not (def-inline? callee))
                (cps-direct env callee x #t #f))
               ((and (eq? (def-result def) 'val) (eq? (def-result callee) 'val))
                (cps-args env callee x
                          (lambda (as)
                            (cps-inline callee as x (lambda (env callee x) (cps-tail env callee x))))))
               (else
                (cps-return env def x)))))

        (else
         (cps-indirect env x #t #f))))))


  (define (cps-return env def x)
    (cps-result env def x (lambda (a) (deliver (return-k) a))))


  ;; (let name ((var init) ...) body ...): a loop continuation, each var a parameter; a call
  ;; (name arg ...) in exit position throws to it
  (define (cps-loop env x last)
    (let-values (((name bindings body) (parse-named-let x)))
      (let loop ((bs bindings) (as '()))
        (if (pair? bs)
            (cps env (cadar bs)
                 (lambda (a)
                   (unless (memq (atom-kind a) '(val raw))
                     (error 'compile-loop "expected a val or a raw" (car bs)))
                   (loop (cdr bs) (cons a as))))
            (let* ((as (reverse as))
                   (k (new-name (symbol->string name)))
                   (params (map (lambda (a) (fresh (atom-kind a))) as))
                   (env (append (map (lambda (b p) (cons (car b) (make-local-binding p)))
                                     bindings params)
                                (acons name (make-loop-binding k params 'here) env))))
              `(letcont-rec ((,k ,(map param params) ,(cps-seq env body last)))
                            (throw ,k ,@as)))))))


  (define (cps-loop-call env x loop)
    (let ((params (loop-binding-slots loop)))
      (unless (= (length (cdr x)) (length params))
        (error 'compile-loop-call "wrong number of arguments" x))
      (cps-exprs env (cdr x) (map atom-kind params) x
                 (lambda (as) `(throw ,(loop-binding-label loop) ,@as)))))


  ;; A named let as a value: each exit throws its value to the continuation after it; the
  ;; enclosing named lets are not in exit position
  (define (cps-loop-value env x c)
    (let* ((j (new-name "j"))
           (k #f)
           (term (cps-loop (disable-loops env 'outer) x
                           (lambda (env y)
                             (cps-exit env y
                                       (lambda (env y)
                                         (cps env y
                                              (lambda (a)
                                                (let ((ak (atom-kind a)))
                                                  (unless (eq? ak 'never)
                                                    (if k
                                                        (check-atom a k x)
                                                        (set! k ak))))
                                                (deliver j a)))))))))
      (if k
          (let* ((r (and (memq k '(val raw)) (fresh k)))
                 (rest (c (or r '(void)))))
            `(letcont ((,j ,(if r (list (param r)) '()) ,rest))
                      ,term))
          (begin
            (c '(never))
            term))))


  ;; The body of def with its parameters bound to the atoms as, in its own env;
  ;; (last env def x) converts the last expression
  (define (cps-inline def as form last)
    (when (memq def (inlining))
      (error 'compile-inline "recursive inline def" (def-name def)))
    (let ((callee-env (map (lambda (name a) (cons name (make-local-binding a)))
                           (def-core-names def) as)))
      (parameterize ((inlining (cons def (inlining)))
                     (unsafe-code? (def-unsafe? def)))
        (cps-seq callee-env (def-core-body def)
                 (lambda (env x) (last env def x))))))


  (define (cps-let env x last)
    (let-values (((bindings body) (parse-let x)))
      (let loop ((bs bindings) (entries '()))
        (if (pair? bs)
            (cps-named env (caar bs) (cadar bs)
                       (lambda (a)
                         (unless (memq (atom-kind a) '(val raw))
                           (error 'compile-let "expected a val or a raw" (car bs)))
                         (loop (cdr bs) (cons (cons (caar bs) (make-local-binding a)) entries))))
            (cps-seq (append entries env) body last)))))


  ;; x, naming it name if it is a closure
  (define (cps-named env name x c)
    (if (eq? (special-keyword x) 'closure)
        (cps-lambda env name x c)
        (cps env x c)))


  ;; Lifts the closure form into its own function, and gives its procedure object, or a
  ;; closure filled with the captures
  (define (cps-lambda env name form c)
    (let* ((captures (lambda-captures env form))
           (def (lambda-def name form captures)))
      (compile-procedure def env)
      (if (null? captures)
          (c `(as-val ,(def-proc def)))
          (call-def (core-def 'alloc-closure) (list (def-proc def)) name
                    (lambda (closure)
                      (fill-captures closure captures name
                                     (lambda () (letprim `(as-val ,closure) 'val c))))))))


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
                    (when (eq? (atom-kind (local-binding-operand (cdr c))) 'raw)
                      (error 'compile-lambda "captured raw variable" (car c))))
                  captures)
        (add-def-symbols! def)
        def)))


  ;; Stores the captures in the closure, a raw from alloc-closure, before the term (k)
  (define (fill-captures closure captures name k)
    (let loop ((captures captures) (i 0))
      (if (null? captures)
          (k)
          (call-def (core-def 'closure-set!)
                    (list closure `(quad ,i) (local-binding-operand (cdar captures)))
                    name
                    (lambda (ignored) (loop (cdr captures) (+ i 1)))))))


  ;; (fix ((name closure) ...) body ...), from the letrec pass: unassigned names bound to
  ;; lambdas, which may refer to each other. The names of those capturing nothing are static
  ;; procedures, called directly. The others are closures: all allocated, their captures set
  ;; to undef so a collection finds no garbage, then filled.
  (define (cps-fix env x last)
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
          (cps-seq env body last)
          (let* ((vars (map (lambda (name) (fresh 'val)) names))
                 (inner (append (map (lambda (name v) (cons name (make-local-binding v)))
                                     names vars)
                                env))
                 (captures (map (lambda (form) (lambda-captures inner form)) forms))
                 (defs (map lambda-def names forms captures)))
            (for-each (lambda (def) (compile-procedure def inner)) defs)
            (let alloc ((ds defs) (cs captures) (vs vars))
              (if (pair? ds)
                  (call-def (core-def 'alloc-closure) (list (def-proc (car ds))) (def-name (car ds))
                            (lambda (closure)
                              (let undef ((i 0))
                                (if (< i (length (car cs)))
                                    (call-def (core-def 'closure-set!)
                                              (list closure `(quad ,i) (immediate 'undef))
                                              (def-name (car ds))
                                              (lambda (ignored) (undef (+ i 1))))
                                    `(letprim ,(car vs) (as-val ,closure)
                                              ,(alloc (cdr ds) (cdr cs) (cdr vs)))))))
                  (let fill ((ds defs) (cs captures) (vs vars))
                    (if (pair? ds)
                        (fill-captures `(as-raw ,(car vs)) (car cs) (def-name (car ds))
                                       (lambda () (fill (cdr ds) (cdr cs) (cdr vs))))
                        (cps-seq inner body last)))))))))


  ;; (check-defined x read [value]): a read of a letrec variable x that may come before its
  ;; init; with a value, from a set! of it, the value, then the check
  (define (cps-check-defined env x c)
    (define (check value)
      (cps env (caddr x)
           (lambda (a)
             (let* ((ok (new-name "k"))
                    (bad (new-name "k"))
                    (error-term (call-def (core-def 'definition-error)
                                          (list (intern-string! (symbol->string (cadr x)))
                                                (intern-string! (if value "assigned" "used")))
                                          x
                                          (lambda (ignored) '(unreachable))))
                    (rest (c (or value a))))
               `(letcont ((,ok () ,rest))
                         (letcont ((,bad () ,error-term))
                                  (if (eq ,a ,(immediate 'unbound)) ,bad ,ok)))))))

    (if (pair? (cdddr x))
        (cps-checked env (cadddr x) 'val x check)
        (check #f)))


  ;; and/or in a test
  (define (cps-and env xs kt kf)
    (cond
     ((null? xs) `(throw ,kt))
     ((null? (cdr xs)) (cps-test env (car xs) kt kf))
     (else
      (let* ((k (new-name "k"))
             (test (cps-test env (car xs) k kf))
             (rest (cps-and env (cdr xs) kt kf)))
        `(letcont ((,k () ,rest)) ,test)))))


  (define (cps-or env xs kt kf)
    (cond
     ((null? xs) `(throw ,kf))
     ((null? (cdr xs)) (cps-test env (car xs) kt kf))
     (else
      (let* ((k (new-name "k"))
             (test (cps-test env (car xs) kt k))
             (rest (cps-or env (cdr xs) kt kf)))
        `(letcont ((,k () ,rest)) ,test)))))


  ;; The library's not, called with one argument
  (define (not-call? env test)
    (let ((b (head-binding env test)))
      (and (def-binding? b)
           (assq 'not (core-defs))
           (eq? (def-binding-def b) (core-def 'not))
           (= (length test) 2))))


  (define (test-true kt kf form)
    (lambda (a)
      (check-atom a 'val form)
      `(if (true ,a) ,kt ,kf)))


  ;; Branches to kt when test is true, else to kf
  (define (cps-test env test kt kf)
    (let ((keyword (special-keyword test)))
      (cond
       ((eq? keyword 'and)
        (cps-and env (cdr test) kt kf))

       ((eq? keyword 'or)
        (cps-or env (cdr test) kt kf))

       ;; Only built by case: two vals compared as words
       ((eq? keyword 'eq?)
        (cps-exprs env (cdr test) '(val val) test (lambda (as) `(if (eq ,@as) ,kt ,kf))))

       ((not-call? env test)
        (cps-test env (cadr test) kf kt))

       ((test-form test)
        => (lambda (name)
             (cps-exprs env (cddr test) '(raw raw) (cdr test)
                        (lambda (as) `(if (,name ,@as) ,kt ,kf)))))

       ((boolean? test)
        `(throw ,(if test kt kf)))

       ((expand-form test)
        => (lambda (x) (cps-test env x kt kf)))

       ((eq? keyword 'if)
        (let-values (((test then else) (parse-if test)))
          (let* ((ka (new-name "k"))
                 (kb (new-name "k"))
                 (test (cps-test env test ka kb))
                 (then (cps-test env then kt kf))
                 (else (cps-test env else kt kf)))
            `(letcont ((,kb () ,else))
                      (letcont ((,ka () ,then))
                               ,test)))))

       ((and (eq? keyword 'let) (not (named-let? test)))
        (cps-let env test (lambda (env x) (cps-test env x kt kf))))

       ((eq? keyword 'begin)
        (cps-seq env (parse-begin test) (lambda (env x) (cps-test env x kt kf))))

       ((and (not keyword)
             (non-empty-list? test)
             (called-def env test))
        => (lambda (def)
             (cond
              ((and (def-inline? def) (eq? (def-result def) 'val))
               (cps-args env def test
                         (lambda (as) (cps-inline def as test (test-last kt kf)))))
              ((def-called? def)
               (cps-direct env def test #f (test-true kt kf test)))
              (else
               (cps-args env def test
                         (lambda (as) (call-def def as test (test-true kt kf test))))))))

       (else
        (cps env test (test-true kt kf test))))))


  (define (cps-if env test then else c)
    (let* ((j (new-name "j"))
           (kt (new-name "k"))
           (kf (new-name "k"))
           (form `(if ,test ,then ,else))
           (ka #f)
           (test (cps-test env test kt kf))
           (then (cps env then
                      (lambda (a)
                        (set! ka (atom-kind a))
                        (deliver j a))))
           (kb #f)
           ;; A branch that does not return takes the kind of the other one
           (else (cps env else
                      (lambda (b)
                        (set! kb (atom-kind b))
                        (unless (or (eq? ka 'never) (eq? kb 'never))
                          (check-atom b ka form))
                        (deliver j b))))
           (k (if (eq? ka 'never) kb ka))
           (inner `(letcont ((,kf () ,else))
                            (letcont ((,kt () ,then))
                                     ,test))))
      (if (eq? k 'never)
          (begin
            (c '(never))
            inner)
          (let* ((r (and (memq k '(val raw)) (fresh k)))
                 (rest (c (or r '(void)))))
            `(letcont ((,j ,(if r (list (param r)) '()) ,rest))
                      ,inner))))))
