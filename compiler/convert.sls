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
  (export compile-procedure compile-low-def compile-unit)
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
          (opt)
          (lower)
          (static))


  ;; Per function: its def, for self tail calls, the kinds of its variables and its return
  ;; continuation
  (define current-function (make-parameter #f))
  (define kinds (make-parameter #f))
  (define return-k (make-parameter #f))


  ;; The dumps number the names (dumped)
  (define (new-name prefix)
    (gensym prefix))


  ;; Compiles the procedure def (a lambda, a procedure define or main) in env, the enclosing
  ;; env, of which it sees the static procedures; its body binds its captures. main first calls
  ;; the startup defs.
  (define (compile-procedure def env)
    (let ((args (def-core-names def))
          (outer (disable-loops (filter (lambda (b) (not (local-binding? (cdr b)))) env) 'lambda)))
      (convert-function
       def (map (lambda (x) (cons x 'val)) args) #t
       (lambda (params)
         (let ((env (append (map (lambda (x p) (cons x (make-local-binding p))) args params)
                            outer)))
           (let loop ((defs (if (string=? (def-symbol def) "main") (startup-defs) '())))
             (if (pair? defs)
                 (call-def (car defs) '() 'main (lambda (ignored) (loop (cdr defs))))
                 (cps-seq env (def-core-body def) (lambda (env x) (cps-tail env def x))))))))))


  ;; A def with typed parameters
  (define (compile-low-def def)
    (convert-function
     def (map cons (def-core-names def) (def-params-kinds def)) #t
     (lambda (params)
       (let ((env (map (lambda (name p) (cons name (make-local-binding p)))
                       (def-core-names def) params)))
         (cps-seq env (def-core-body def) (lambda (env x) (cps-tail env def x)))))))


  ;; During compile-unit, the functions of the compilation converted but not lowered yet: the
  ;; element 0 of a vector, each (def . fun), the last converted first
  (define current-unit (make-parameter #f))


  ;; Calls thunk, which converts the functions of the compilation; then optimizes each, runs the
  ;; passes on the whole unit (optimize-unit) and lowers each left, in the order of their
  ;; conversion. Gives ((def . fun) ...), the funs left after the unit passes.
  (define (compile-unit thunk)
    (let ((unit (vector '())))
      (parameterize ((current-unit unit))
        (thunk))
      (let* ((funs (reverse (vector-ref unit 0)))
             (optimized (map (lambda (f) (optimize (car f) (cdr f))) funs))
             (final (optimize-unit (map car funs) optimized)))
        (for-each (lambda (f fun) (when fun (finish-function (car f) fun)))
                  funs final)
        (filter cdr (map (lambda (f fun) (cons (car f) fun)) funs final)))))


  ;; The function of def, its parameters params, (name . kind) pairs, its body the term
  ;; (make-term vars) of the parameters' variables, converted and checked; then optimized when
  ;; optimize?, and lowered, at once or with the rest of the unit
  (define (convert-function def params optimize? make-term)
    (let ((fun (parameterize ((unsafe-code? (def-unsafe? def))
                              (current-function def)
                              (kinds (make-eq-hashtable))
                              (return-k (gensym "return")))
                 (let* ((vars (map-in-order (lambda (p) (fresh-named (car p) (cdr p))) params))
                        (fun `(fun ,(def-code def) (,(return-k) ,@(map param vars)) ,(make-term vars))))
                   (dumped 'cps (list-head fun 3) (cdddr fun))
                   (check-cps fun)
                   fun))))
      (cond
       ((not optimize?) (finish-function def fun))
       ((current-unit)
        (vector-set! (current-unit) 0 (cons (cons def fun) (vector-ref (current-unit) 0))))
       (else (finish-function def (optimize def fun))))))


  ;; The frames of fun, then its lowering
  (define (finish-function def fun)
    (emit-function!
     def
     (lambda ()
       (parameterize ((unsafe-code? (def-unsafe? def))
                      (current-function def)
                      (kinds (cps-kinds fun))
                      (return-k (car (caddr fun))))
         (let ((fun `(,@(list-head fun 3) ,(expand-frames (cadddr fun)))))
           (dumped 'frames (list-head fun 3) (cdddr fun))
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
    (fresh-named 't kind))


  ;; A variable named after the symbol name
  (define (fresh-named name kind)
    (let ((x (new-name (symbol->string name))))
      (hashtable-set! (kinds) x kind)
      x))


  (define (atom-kind a)
    (if (symbol? a)
        (or (hashtable-ref (kinds) a #f) (compile-error 'convert "variable without a kind" a))
        (kind a)))


  ;; As check-kind
  (define (check-atom a k form)
    (let ((ak (atom-kind a)))
      (unless (if (eq? k 'any) (memq ak '(val raw)) (eq? ak k))
        (compile-error 'check-kind (if (eq? k 'any) "expected a val or a raw" (format "expected a ~a" k))
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


  ;; The literal x, a datum, a static object (static.sls), given to c: its label as a val, or
  ;; plus 7 for a pair
  (define (literal-atom x c)
    (let ((r (place-static! x)))
      (cond
       ((string? r) (c `(as-val ,r)))
       ((number? r) (c `(as-val (quad ,r))))
       (else
        (letprim `(raw+ ,(cadr r) (quad ,(caddr r))) 'raw
                 (lambda (t) (letprim `(as-val ,t) 'val c)))))))


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
        (compile-error 'lookup "unknown symbol" x))
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


  ;; The def a call expr calls with the right number of arguments, or #f; another number is an
  ;; error, except for a procedure, which checks it when called
  (define (called-def env expr)
    (let ((b (head-binding env expr))
          (n (length (cdr expr))))
      (and (def-binding? b)
           (let ((def (def-binding-def b)))
             (cond
              ((takes? def n) def)
              ((def-procedure? def) #f)
              (else (compile-error 'compile-call "wrong number of arguments" expr)))))))


  ;; case-lambda groups (defs.sls).
  ;; A call of a group goes to its first clause taking its number of operands, a rest clause
  ;; getting the extra ones as a fresh list. An inline rest clause is expanded instead: when its
  ;; rest parameter is only the last operand of applies, bound to (spliced atom ...), which such
  ;; an apply spreads as its operands, else to the fresh list.
  ;; An apply of a group walks the list to the clause taking the arguments.

  ;; The call x in simpler terms, as (env . x): an apply of a spliced list or of a group, or a
  ;; group call that is not inline; else #f
  (define (case-rewrite env x)
    (let ((b (head-binding env x)))
      (and (def-binding? b)
           (let ((def (def-binding-def b)))
             (cond
              ((and (eq? def (core-def 'apply)) (>= (length x) 3))
               (let ((f (cadr x))
                     (es (list-head (cddr x) (- (length x) 3)))
                     (l (car (last-pair x))))
                 (cond
                  ((spliced-operand env l)
                   => (lambda (as)
                        (let ((ts (map-in-order (lambda (a) (gensym "spliced")) as)))
                          (cons (append (map (lambda (t a) (cons t (make-local-binding a))) ts as) env)
                                `(,f ,@es ,@ts)))))
                  ((group-operand env f)
                   => (lambda (g) (cons env (apply-tree g es l))))
                  (else #f))))
              ((def-clauses def)
               => (lambda (clauses)
                    (let ((c (first-clause clauses (length (cdr x)))))
                      (cond
                       ((not c) #f)
                       ((not (clause-rest? c)) (cons env (cons (clause-def c) (cdr x))))
                       ((inline-clause? c) #f)
                       (else
                        (let ((r (clause-required c)))
                          (cons env `(,(clause-def c) ,@(list-head (cdr x) r)
                                      ,(cons-chain (list-tail (cdr x) r) ''())))))))))
              (else #f))))))


  ;; The inline rest clause a group call x is expanded into, or #f
  (define (inline-rest-clause env x)
    (let ((b (head-binding env x)))
      (and (def-binding? b)
           (let ((clauses (def-clauses (def-binding-def b))))
             (and clauses
                  (let ((c (first-clause clauses (length (cdr x)))))
                    (and c (clause-rest? c) (inline-clause? c) (clause-def c))))))))


  ;; An inline rest clause is expanded with its list spliced, or else fresh, but not inside
  ;; itself
  (define (inline-clause? c)
    (and (clause-inline? c)
         (or (spliceable? c) (not (assq (clause-def c) (inlining))))))


  ;; The atoms of x's operands, those past def's required parameters as one spliced atom, or
  ;; as a fresh list
  (define (cps-rest-args env def x c)
    (let* ((r (- (length (def-params def)) 1))
           (required (list-head (cdr x) r)))
      (if (spliceable? (def-clause def))
          (cps-exprs env (cdr x) (map (lambda (y) 'val) (cdr x)) x
                     (lambda (as) (c (append (list-head as r) (list (cons 'spliced (list-tail as r)))))))
          (cps-exprs env required (map (lambda (y) 'val) required) x
                     (lambda (as)
                       (cps-rest-list env (list-tail (cdr x) r) x
                                      (lambda (l) (c (append as (list l))))))))))


  (define (spliced? a)
    (and (pair? a) (eq? (car a) 'spliced)))


  (define (spliced-call? as)
    (and (pair? as) (spliced? (car (last-pair as)))))


  ;; The atoms of the spliced rest list x names, or #f
  (define (spliced-operand env x)
    (and (symbol? x)
         (let ((entry (assq x env)))
           (and entry
                (local-binding? (cdr entry))
                (spliced? (local-binding-operand (cdr entry)))
                (cdr (local-binding-operand (cdr entry)))))))


  ;; The group x names, or #f
  (define (group-operand env x)
    (let ((b (cond
              ((def? x) (make-def-binding x))
              ((symbol? x) (let ((entry (assq x env))) (and entry (cdr entry))))
              (else #f))))
      (and (def-binding? b)
           (def-clauses (def-binding-def b))
           (def-binding-def b))))


  ;; Whether the rest clause c is spliced: inline, its rest parameter only the last operand of
  ;; applies (so not captured either)
  (define (spliceable? c)
    (and (clause-inline? c)
         (let ((r (car (last-pair (def-core-names (clause-def c)))))
               (apply-def (core-def 'apply)))
           (let walk ((x (def-core-body (clause-def c))))
             (cond
              ((eq? x r) #f)
              ((not (pair? x)) #t)
              ((and (eq? (car x) apply-def) (eq? (car (last-pair x)) r))
               (walk (list-head x (- (length x) 1))))
              (else (and (walk (car x)) (walk (cdr x)))))))))


  ;; (cons x (cons ... tail))
  (define (cons-chain xs tail)
    (fold-right (lambda (x acc) (list (core-def 'cons) x acc)) tail xs))


  ;; (apply g e ... l) as a walk of l's spine to the clause of g taking the arguments: past the
  ;; counts a fixed clause takes, the rest clause gets the tail, shared when l is the current
  ;; function's own rest list, used only there, else a checked copy. The elements an own list
  ;; is known to have are taken without tests.
  (define (apply-tree g es l)
    (let* ((clauses (def-clauses g))
           (top (apply max (map clause-required clauses)))
           (gs (map-in-order (lambda (e) (gensym "a")) es))
           (lg (gensym "l"))
           (known (own-list l)))
      (define (leaf c atoms tail)
        (let ((r (clause-required c)))
          (if (clause-rest? c)
              `(,(clause-def c) ,@(list-head atoms r) ,(cons-chain (list-tail atoms r) tail))
              `(,(clause-def c) ,@atoms))))

      (define fail
        `(,(core-def 'apply-error) ,g ,(length gs) ,lg))

      ;; The clause every count past p reaches, if a rest clause of at most p required ones;
      ;; none when no clause takes any; else #f
      (define (beyond p)
        (let ((cs (map (lambda (i) (first-clause clauses (+ p 1 i)))
                       (iota (- (max (+ top 1) (+ p 1)) p)))))
          (cond
           ((for-all not cs) 'none)
           ((and (car cs)
                 (for-all (lambda (c) (eq? c (car cs))) cs)
                 (clause-rest? (car cs))
                 (<= (clause-required (car cs)) p))
            (car cs))
           (else #f))))

      ;; p atoms, then the list t of at least k elements
      (define (node p atoms t k)
        (define (past)
          (let ((c (beyond p)))
            (cond
             ((eq? c 'none) fail)
             (c (leaf c atoms (if known t `(,(core-def 'apply-list-copy) ,t ,lg))))
             (else
              (let* ((a (gensym "a"))
                     (u (gensym "l"))
                     (next `(,(marker 'let) ((,a (,(core-def 'pair-car) ,t))
                                             (,u (,(core-def 'pair-cdr) ,t)))
                             ,(node (+ p 1) (append atoms (list a)) u (max (- k 1) 0)))))
                (if (> k 0)
                    next
                    `(,(marker 'if) (,(core-def 'pair?) ,t) ,next ,fail)))))))
        (if (> k 0)
            (past)
            `(,(marker 'if) (,(core-def 'null?) ,t)
              ,(let ((c (first-clause clauses p)))
                 (if c (leaf c atoms t) fail))
              ,(past))))

      `(,(marker 'let) (,@(map list gs es) (,lg ,l))
        ,(node (length gs) gs lg (or known 0)))))


  ;; When x is the current function's rest list, used only once, so a callee may share it, the
  ;; number of elements it has at least, else #f: a dispatcher's list may be empty, a rest
  ;; clause's has those past its required parameters of the fewest arguments it is called with
  (define (own-list x)
    (let* ((f (current-function))
           (c (def-clause f)))
      (and (symbol? x)
           (or (def-clauses f) (and c (clause-rest? c)))
           (eq? x (car (last-pair (def-core-names f))))
           (= (let count ((y (def-core-body f)))
                (cond
                 ((eq? y x) 1)
                 ((pair? y) (+ (count (car y)) (count (cdr y))))
                 (else 0)))
              1)
           (if c
               (let ((clauses (def-clauses (clause-group f))))
                 (let loop ((n (clause-required c)))
                   (if (eq? (first-clause clauses n) c)
                       (- n (clause-required c))
                       (loop (+ n 1)))))
               0))))


  (define (immediate-operand x)
    (immediate (cond ((eq? x undef) 'undef) ((eq? x unbound) 'unbound) (else x))))


  (define (cps env x c)
    (cond
     ((immediate? x)
      (c (immediate-operand x)))

     ((exact-rational? x)
      (c `(as-val ,(intern-number! x))))

     ((number? x)
      (compile-error 'compile-value "inexact numbers are not supported" x))

     ;; Outside unsafe code, a Scheme string, loaded from its literal cell
     ((string? x)
      (if (unsafe-code?)
          (c (intern-string! x))
          (literal-atom x c)))

     ((literal? x)
      (literal-atom (literal-datum x) c))

     ((symbol? x)
      (cps-variable env x c))

     ((def? x)
      (c (def-value x (def-name x))))

     ;; A data name gives the raw address of its first word
     ((data? x)
      (c (data-symbol x)))

     ((expand-form x)
      => (lambda (y) (cps env y c)))

     ((special-keyword x)
      => (lambda (keyword) (cps-special env keyword x c)))

     ((non-empty-list? x)
      (cps-call env x c))

     (else
      (compile-error 'compile-value "unsupported expression" x))))


  (define (cps-variable env x c)
    (let ((b (lookup env x)))
      (cond
       ((local-binding? b)
        (let ((a (local-binding-operand b)))
          (cond
           ((deferred-literal? a) (literal-atom (cadr a) c))
           ((spliced? a) (compile-error 'compile-value "spliced rest list used as a value" x))
           (else (c a)))))
       ;; A static procedure of a fix
       ((def-binding? b)
        (c (def-value (def-binding-def b) x)))
       (else
        (loop-error b x)))))


  (define (loop-error b x)
    (if (eq? (loop-binding-state b) 'lambda)
        (compile-error 'compile-value "named let called from a lambda" x)
        (compile-error 'compile-value "named let used as a value or called not in exit position" x)))


  ;; The procedure object of def
  (define (def-value def x)
    (unless (def-emitted? def)
      (compile-error 'compile-value "inline def without a procedure object" x))
    (when (def-noalloc? def)
      (compile-error 'compile-value "noalloc def without a procedure object" x))
    `(as-val ,(def-proc def)))


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
                (compile-error 'compile-value "expected a val or a raw" x))
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
         (compile-error 'compile-value "invalid when or unless" x))
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
       (compile-error 'compile-value "unsupported form" x))))


  (define (cps-call env x c)
    (let ((b (head-binding env x)))
      (cond
       ((loop-binding? b)
        (loop-error b x))
       ((case-rewrite env x)
        => (lambda (r) (cps (car r) (cdr r) c)))
       ((inline-rest-clause env x)
        => (lambda (def) (cps-rest-args env def x (lambda (as) (inline-call def as x c)))))
       ((called-def env x)
        => (lambda (def)
             (if (def-called? def)
                 (cps-direct env def x #f c)
                 (cps-args env def x (lambda (as) (call-def def as x c))))))
       (else
        (cps-indirect env x #f c)))))


  ;; Calls def with the atoms as, its parameters, giving its value to c
  (define (call-def def as form c)
    (cond
     ((def-inline? def)
      (inline-call def as form c))
     ;; A call on the machine stack, so not a safepoint
     ((def-noalloc? def)
      (letprim `(stackcall ,(def-result def) ,def ,@as) (def-result def) c))
     ((def-rest? def)
      (compile-error 'call-def "def with a rest parameter called with its parameters" (def-name def)))
     (else
      (app-to #f def as (def-result def) c))))


  (define (inline-call def as form c)
    (let ((c (outside c)))
      (cps-inline def as form (lambda (env def x) (cps-result env def x c)))))


  ;; (app tag callee k atom ...), its value, of kind result, given to c through k
  (define (app-to tag callee as result c)
    (if (eq? result 'never)
        (begin
          (c '(never))
          `(app ,tag ,callee never ,@as))
        (let* ((k (new-name "k"))
               (r (fresh result))
               (body (c r)))
          `(letcont ((,k (,(param r)) ,body))
                    (app ,tag ,callee ,k ,@as)))))


  ;; The call (def arg ...) of a def-called? def, a tail call when tail?: its arguments' code
  ;; marked, so the frame is allocated before it (expand-frames)
  (define (cps-direct env def x tail? c)
    (let ((tag (new-name "a")))
      `(args ,tag
             ,(cps-exprs env (cdr x) (map (lambda (y) 'val) (cdr x)) x
                         (lambda (as)
                           (if tail?
                               `(app ,tag ,def ,(return-k) ,@as)
                               (app-to tag def as (def-result def) c)))))))


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
             (compile-error 'compile-value "invalid raw literal" x))
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
             (compile-error 'compile-value "invalid syscall" x))
           (cps-exprs env (cddr x) (map (lambda (y) 'raw) (cddr x)) x
                      (lambda (as) (letprim `(syscall (quad ,(cadr x)) ,@as) 'raw c))))

          ((alloc)
           (cps-operands env x (op-operands op)
                         (lambda (as)
                           (let ((n (car as)))
                             (when (and (pair? n) (eq? (car n) 'quad) (<= (cadr n) 0))
                               (compile-error 'compile-value "invalid alloc" x)))
                           (letprim `(alloc ,@as) 'raw c))))

          ((tail-call-frame)
           (cps-operands env x (op-operands op)
                         (lambda (as)
                           (c '(never))
                           `(call ,(return-k) frame ,(car as) #f))))

          ((self)
           (cps-operands env x (op-operands op) (lambda (as) (c '(self)))))

          ((address)
           (unless (literal? string?)
             (compile-error 'compile-value "invalid address" x))
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
      (compile-error 'compile-exprs "wrong number of operands" form))
    (let loop ((xs xs) (kinds kinds) (as '()))
      (if (null? xs)
          (c (reverse as))
          (cps-checked env (car xs) (car kinds) form
                       (lambda (a) (loop (cdr xs) (cdr kinds) (cons a as)))))))


  ;; The atoms of a call to def, the extra ones packed into its rest parameter's list
  (define (cps-args env def x c)
    (cond
     ((def-rest? def)
      (let ((required (list-head (cdr x) (def-required def))))
        (cps-exprs env required (map (lambda (y) 'val) required) x
                   (lambda (as)
                     (cps-rest-list env (list-tail (cdr x) (def-required def)) x
                                    (lambda (l) (c (append as (list l)))))))))
     ((def-inline? def)
      (cps-inline-args env def x c))
     (else
      (cps-exprs env (cdr x) (def-params-kinds def) x c))))


  ;; As cps-exprs, but a literal, or a variable bound to one, stays unloaded: the parameter is
  ;; bound to (deferred-literal datum), loaded at each use (cps-variable), so a literal only an
  ;; inline def's error path uses is loaded there
  (define (cps-inline-args env def x c)
    (define (deferred x)
      (cond
       ((literal? x) `(deferred-literal ,(literal-datum x)))
       ((and (string? x) (not (unsafe-code?))) `(deferred-literal ,x))
       ((symbol? x)
        (let ((b (lookup env x)))
          (and (local-binding? b)
               (deferred-literal? (local-binding-operand b))
               (local-binding-operand b))))
       (else #f)))

    (let ((kinds (def-params-kinds def)))
      (unless (= (length (cdr x)) (length kinds))
        (compile-error 'compile-exprs "wrong number of operands" x))
      (let loop ((xs (cdr x)) (kinds kinds) (as '()))
        (cond
         ((null? xs)
          (c (reverse as)))
         ((and (eq? (car kinds) 'val) (deferred (car xs)))
          => (lambda (a) (loop (cdr xs) (cdr kinds) (cons a as))))
         (else
          (cps-checked env (car xs) (car kinds) x
                       (lambda (a) (loop (cdr xs) (cdr kinds) (cons a as)))))))))


  (define (deferred-literal? a)
    (and (pair? a) (eq? (car a) 'deferred-literal)))


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
          (self (current-function))
          (tag (new-name "a")))
      (cps-checked
       env (car x) 'val x
       (lambda (proc)
         (define (args tag)
           (cps-exprs env (cdr x) (map (lambda (y) 'val) (cdr x)) x
                      (lambda (as)
                        (if tail?
                            `(app ,tag ,proc ,(return-k) ,@as)
                            (app-to tag proc as 'val c)))))

         (if (and tail? (not (def-rest? self)) (= n (length (def-params self))))
             (args #f)
             `(args ,tag ,(args tag)))))))


  ;; The inline def's last expression as a test
  (define (test-last kt kf)
    (lambda (env def x)
      (cps-test env x kt kf)))


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
             (immediate? x))
         (cps-return env def x))

        ((case-rewrite env x)
         => (lambda (r) (cps-tail (car r) def (cdr r))))

        ((inline-rest-clause env x)
         => (lambda (callee)
              (if (and (eq? (def-result def) 'val) (eq? (def-result callee) 'val))
                  (cps-rest-args env callee x
                                 (lambda (as)
                                   (cps-inline callee as x (lambda (env callee y) (cps-tail env callee y)))))
                  (cps-return env def x))))

        ((called-def env x)
         => (lambda (callee)
              (cond
               ((def-noalloc? callee)
                (cps-return env def x))
               ((eq? callee (current-function))
                (cps-exprs env (cdr x) (map (lambda (y) 'val) (cdr x)) x
                           (lambda (as) `(app #f ,callee ,(return-k) ,@as))))
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
                     (compile-error 'compile-loop "expected a val or a raw" (car bs)))
                   (loop (cdr bs) (cons a as))))
            (let* ((as (reverse as))
                   (k (new-name (symbol->string name)))
                   (params (map-in-order (lambda (a) (fresh (atom-kind a))) as))
                   (env (append (map (lambda (b p) (cons (car b) (make-local-binding p)))
                                     bindings params)
                                (acons name (make-loop-binding k params 'here) env))))
              `(letcont-rec ((,k ,(map param params) ,(cps-seq env body last)))
                            (throw ,k ,@as)))))))


  (define (cps-loop-call env x loop)
    (let ((params (loop-binding-slots loop)))
      (unless (= (length (cdr x)) (length params))
        (compile-error 'compile-loop-call "wrong number of arguments" x))
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
  ;; (last env def x) converts the last expression.
  ;; A def is expanded inside itself only through a spliced rest list, with fewer arguments.
  (define (cps-inline def as form last)
    (let ((n (and (spliced-call? as) (+ (length as) -1 (length (cdr (car (last-pair as)))))))
          (callee-env (map (lambda (name a) (cons name (make-local-binding a)))
                           (def-core-names def) as)))
      (when (exists (lambda (e) (and (eq? (car e) def) (not (and n (cdr e) (< n (cdr e))))))
                    (inlining))
        (compile-error 'compile-inline "recursive inline def" (def-name def)))
      (parameterize ((inlining (cons (cons def n) (inlining)))
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
                           (compile-error 'compile-let "expected a val or a raw" (car bs)))
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
               (compile-error 'compile-lambda "capture not a local" var))
             entry))
         (cadr form)))


  ;; The def of a closure form, with its symbols
  (define (lambda-def name form captures)
    (let-values (((capture-names args body) (parse-closure form)))
      (let ((def (make-def (or name 'lambda) (mangle (fresh-name (or name 'lambda)))
                           (formals->params args) 'val
                           #f #f #f #f #t (unsafe-code?) (length captures) (not name) #f body
                           #f (cons (formals-names args) body))))
        (for-each (lambda (c)
                    (when (eq? (atom-kind (local-binding-operand (cdr c))) 'raw)
                      (compile-error 'compile-lambda "captured raw variable" (car c))))
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
           (defs (map-in-order (lambda (b) (lambda-def (car b) (cadr b) '())) static))
           (env (append (map (lambda (b def) (cons (car b) (make-def-binding def))) static defs)
                        env))
           (closures (filter (lambda (b) (pair? (cadadr b))) (cadr x)))
           (names (map car closures))
           (forms (map cadr closures))
           (body (cddr x)))
      (for-each (lambda (def) (compile-procedure def env)) defs)
      (if (null? closures)
          (cps-seq env body last)
          (let* ((vars (map-in-order (lambda (name) (fresh 'val)) names))
                 (inner (append (map (lambda (name v) (cons name (make-local-binding v)))
                                     names vars)
                                env))
                 (captures (map (lambda (form) (lambda-captures inner form)) forms))
                 (defs (map-in-order lambda-def names forms captures)))
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
      (if (eq? (atom-kind a) 'never)
          '(unreachable)
          (begin
            (check-atom a 'val form)
            `(if (true ,a) ,kt ,kf)))))


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
             (case-rewrite env test))
        => (lambda (r) (cps-test (car r) (cdr r) kt kf)))

       ((and (not keyword)
             (non-empty-list? test)
             (inline-rest-clause env test))
        => (lambda (def)
             (if (eq? (def-result def) 'val)
                 (cps-rest-args env def test (lambda (as) (cps-inline def as test (test-last kt kf))))
                 (cps env test (test-true kt kf test)))))

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
                      ,inner)))))


  ;; Frames, the last step of the conversion: each (app tag callee k atom ...) to the block IR's
  ;; calls, whose caller allocates the callee's frame and stores the arguments in it. A marked
  ;; call's frame, (args tag term), is allocated at the mark, before its arguments' code, and
  ;; each argument stored as soon as the variable holding it is bound, so only the frame is
  ;; live across the calls the arguments make; an unmarked call's frame is allocated at the app.
  ;; A tail call of the function itself reuses its frame (selfcall), as may an unmarked tail call
  ;; of an unknown procedure taking as many arguments, after checking its code.

  ;; A marked call being filled: its frame, the index of the next argument to store, the number
  ;; stored before the call (a rest def's required ones), and its atoms
  (define-record-type filling
    (fields frame next limit atoms))


  (define (expand-frames term)
    (let ((apps (make-eq-hashtable))
          (positions (make-eq-hashtable))
          (spans (make-eq-hashtable))
          (n 0))
      ;; The marks' apps, and the preorder positions of the bindings and of the marks' terms
      (define (number! t)
        (define (position! x)
          (hashtable-set! positions x n)
          (set! n (+ n 1)))

        (case (car t)
          ((letprim)
           (when (cadr t)
             (position! (cadr t)))
           (number! (cadddr t)))
          ((letcont letcont-rec)
           (let ((b (car (cadr t))))
             (for-each position! (map param-var (cadr b)))
             (number! (caddr b))
             (number! (caddr t))))
          ((args)
           (let ((start n))
             (number! (caddr t))
             (hashtable-set! spans (cadr t) (cons start n))))
          ((app)
           (when (cadr t)
             (hashtable-set! apps (cadr t) t)))
          (else (void))))

      ;; Whether the atom a is computed before the arguments of the call tag: not a variable
      ;; bound inside its mark
      (define (ready? tag a)
        (let ((p (and (symbol? a) (hashtable-ref positions a #f)))
              (span (hashtable-ref spans tag #f)))
          (not (and p (<= (car span) p) (< p (cdr span))))))

      ;; pending: the marked calls being filled on this path, (tag . filling)
      (define (walk t pending)
        (case (car t)
          ((letprim)
           `(letprim ,(cadr t) ,(caddr t)
                     ,(bound (if (cadr t) (list (cadr t)) '()) pending
                             (lambda (pending) (walk (cadddr t) pending)))))
          ((letcont letcont-rec)
           (let* ((b (car (cadr t)))
                  (body (bound (map param-var (cadr b)) pending
                               (lambda (pending) (walk (caddr b) pending)))))
             `(,(car t) ((,(car b) ,(cadr b) ,body))
               ,(walk (caddr t) pending))))
          ((args)
           (let ((app (hashtable-ref apps (cadr t) #f)))
             (if app
                 (let ((callee (caddr app))
                       (as (cddddr app)))
                   (allocate-frame callee (length as)
                                   (lambda (f)
                                     (store-ready (cadr t) (make-filling f 0 (limit callee as) as) #f
                                                  (lambda (filling)
                                                    (zero-pending filling (safepoint-in? (cadr t) (caddr t))
                                                                  (lambda ()
                                                                    (walk (caddr t)
                                                                          (acons (cadr t) filling pending)))))))))
                 (walk (caddr t) pending))))
          ((app)
           (expand-app t pending))
          (else t)))

      ;; The argument slots of the filling f not stored yet zeroed, when a safepoint may come
      ;; before them (safepoint?), then (k): the GC scans a pending frame's arguments, and the
      ;; frame region is not zeroed
      (define (zero-pending f safepoint? k)
        (let loop ((i (filling-next f)))
          (if (and safepoint? (< i (filling-limit f)))
              (frame-store (filling-frame f) i '(quad 0) (lambda () (loop (+ i 1))))
              (k))))

      ;; Whether the term t, computing the arguments of the call tag, may reach a safepoint
      (define (safepoint-in? tag t)
        (let walk ((t t))
          (case (car t)
            ((letprim) (or (eq? (cps-effect (caddr t)) 'allocates) (walk (cadddr t))))
            ((letcont letcont-rec) (or (walk (caddr (car (cadr t)))) (walk (caddr t))))
            ((args) (walk (caddr t)))
            ((app) (not (eq? (cadr t) tag)))
            ((call) #t)
            (else #f))))

      ;; The variables vars bound: the arguments they hold stored, then (k pending)
      (define (bound vars pending k)
        (let loop ((ps pending) (done '()))
          (if (null? ps)
              (k (reverse done))
              (let* ((tag (caar ps))
                     (f (cdar ps))
                     (i (filling-next f)))
                (if (and (< i (filling-limit f)) (memq (list-ref (filling-atoms f) i) vars))
                    (store-ready tag f #t
                                 (lambda (f) (loop (cdr ps) (cons (cons tag f) done))))
                    (loop (cdr ps) (cons (car ps) done)))))))

      ;; Stores the next argument when first?, then each following one computed before the call,
      ;; then (k filling)
      (define (store-ready tag f first? k)
        (let loop ((i (filling-next f)) (first? first?))
          (if (and (< i (filling-limit f))
                   (or first? (ready? tag (list-ref (filling-atoms f) i))))
              (frame-store (filling-frame f) i (list-ref (filling-atoms f) i)
                           (lambda () (loop (+ i 1) #f)))
              (k (make-filling (filling-frame f) i (filling-limit f) (filling-atoms f))))))

      (define (expand-app t pending)
        (let* ((tag (cadr t))
               (callee (caddr t))
               (k (cadddr t))
               (as (cddddr t))
               (n (length as))
               (self (current-function))
               (entry (and tag (assq tag pending))))
          (cond
           (entry
            (fill-rest callee (cdr entry) (lambda () (call-frame callee k (filling-frame (cdr entry)) n))))
           ((and (eq? callee self) (eq? k (return-k)))
            (with-rest-list callee as (lambda (as) `(selfcall (self) ,@as))))
           ((and (not (def? callee)) (eq? k (return-k))
                 (not (def-rest? self)) (= n (length (def-params self))))
            (let* ((other (new-name "k"))
                   (same (new-name "k"))
                   (test (cps-inline (core-def 'same-code?) (list callee (def-code self)) '()
                                     (test-last same other)))
                   (call (allocate-frame callee n
                                         (lambda (f)
                                           (fill-rest callee (make-filling f 0 n as)
                                                      (lambda () (call-frame callee k f n)))))))
              `(letcont ((,other () ,call))
                        (letcont ((,same () (selfcall ,callee ,@as)))
                                 ,test))))
           (else
            (allocate-frame callee n
                            (lambda (f)
                              (fill-rest callee (make-filling f 0 (limit callee as) as)
                                         (lambda () (call-frame callee k f n)))))))))

      (number! term)
      (walk term '())))


  ;; The number of arguments stored as they are computed: all but a rest def's extra ones
  (define (limit callee as)
    (if (and (def? callee) (def-rest? callee)) (def-required callee) (length as)))


  ;; The frame of a call of callee, a def or a procedure value, with n arguments
  (define (allocate-frame callee n c)
    (if (def? callee)
        (direct-frame callee c)
        (call-def (core-def 'make-call-frame) (list callee `(quad ,n)) '() c)))


  (define (direct-frame def c)
    (call-def (core-def 'make-direct-frame) (list (def-code def) `(as-val ,(def-proc def))) '() c))


  ;; The arguments of f not stored yet, then a rest def's extra ones as a fresh list, before (k);
  ;; its slot zeroed while the list is consed, a safepoint
  (define (fill-rest callee f k)
    (let ((frame (filling-frame f))
          (as (filling-atoms f))
          (limit (filling-limit f)))
      (let loop ((i (filling-next f)))
        (cond
         ((< i limit)
          (frame-store frame i (list-ref as i) (lambda () (loop (+ i 1)))))
         ((and (def? callee) (def-rest? callee))
          (let ((store-rest (lambda ()
                              (rest-list (list-tail as limit) (lambda (l) (frame-store frame limit l k))))))
            (if (null? (list-tail as limit))
                (store-rest)
                (frame-store frame limit '(quad 0) store-rest))))
         (else
          (k))))))


  ;; The atoms of a call of def, the extra ones packed into its rest parameter's list
  (define (with-rest-list def as c)
    (if (def-rest? def)
        (let ((r (def-required def)))
          (rest-list (list-tail as r) (lambda (l) (c (append (list-head as r) (list l))))))
        (c as)))


  ;; A fresh list of the atoms as, consed from the last
  (define (rest-list as c)
    (let loop ((as (reverse as)) (acc (immediate-operand ''())))
      (if (null? as)
          (c acc)
          (call-def (core-def 'cons) (list (car as) acc) '() (lambda (p) (loop (cdr as) p))))))


  (define (frame-store f i a k)
    `(letprim #f (store (as-raw ,f) (quad ,(+ i 6)) ,a) ,(k)))


  ;; The call of callee, with n arguments, through its filled frame f; a procedure value's frame
  ;; is a placeholder when its arity is not n, which apply-frame turns into its frame
  (define (call-frame callee k f n)
    (if (def? callee)
        `(call ,k ,(def-code callee) ,f ,(if (def-rest? callee) (+ (def-required callee) 1) n))
        (let* ((fast (new-name "k"))
               (slow (new-name "k"))
               (test (cps-inline (core-def 'frame-fits?) (list f `(quad ,n)) '() (test-last fast slow)))
               (apply-frame (core-def 'apply-frame))
               (slow-term (direct-frame apply-frame
                                        (lambda (g)
                                          (frame-store g 0 f
                                                       (lambda ()
                                                         (if (eq? k (return-k))
                                                             `(call ,k ,(def-code apply-frame) ,g 1)
                                                             (let* ((k2 (new-name "k"))
                                                                    (r (fresh 'val)))
                                                               `(letcont ((,k2 (,r) (throw ,k ,r)))
                                                                         (call ,k2 ,(def-code apply-frame) ,g 1))))))))))
          `(letcont ((,slow () ,slow-term))
                    (letcont ((,fast () (call ,k frame ,f ,n)))
                             ,test))))))
