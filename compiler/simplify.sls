;; The simplifier, shrinking reductions on a fun, round after round until none applies:
;; - dead code: a letprim of a pure or reading op whose variable is unused, a continuation never
;;   used
;; - continuations: one thrown to once is substituted at the throw; one only throwing its
;;   parameters on, (j (r) (throw k r)), is k
;; - constant folding: pure low ops on constants, casts of casts in atoms, the ifs whose test is
;;   known (constants, or a variable compared with itself)
;; - jump threading: a throw to a continuation whose body is an if on its parameters goes
;;   straight to the branch when the thrown atoms decide the test
;; - invariant parameters: a continuation only thrown to, each throw passing one parameter the
;;   same atom bound outside it (or, in a loop, the parameter itself), loses the parameter
;; - algebra: (raw+ x (quad 0)), (bit-and x (quad -1))... are x
;; - dead objects: an alloc whose object is only written into (stores, copy-words), or stored
;;   into such objects, goes with those writes (a lifted closure's)
;; Each round walks the term once with the census taken before it; a reduction that makes others
;; possible leaves them to the next round.

(library (simplify)
  (export simplify compare quad? word)
  (import (chezscheme)
          (utils)
          (defs)
          (ops)
          (output)
          (cps))


  (define max-rounds 32)


  (define (simplify def fun)
    (let loop ((fun fun) (round 1))
      (let-values (((fun changed?) (simplify-round def fun)))
        (if (and changed? (< round max-rounds))
            (loop fun (+ round 1))
            fun))))


  ;; Per continuation, its throws (and their atoms in thrown) and whether an if or a call uses
  ;; it; the cells, targets of a cell-set!; the preorder positions of the binders, and of each
  ;; continuation's span, its binding (start . end)
  (define (count-cont-uses! t throws thrown branched called cells positions spans)
    (define (count! table k)
      (hashtable-update! table k (lambda (n) (+ n 1)) 0))

    (define n 0)

    (define (position! x)
      (hashtable-set! positions x n)
      (set! n (+ n 1)))

    (let walk ((t t))
      (case (car t)
        ((letprim)
         (when (eq? (car (caddr t)) 'cell-set!)
           (hashtable-set! cells (cadr (caddr t)) #t))
         (when (cadr t)
           (position! (cadr t)))
         (walk (cadddr t)))
        ((letcont letcont-rec)
         (let ((start n)
               (b (car (cadr t))))
           (for-each position! (map param-var (cadr b)))
           (walk (caddr b))
           (walk (caddr t))
           (hashtable-set! spans (car b) (cons start n))))
        ((if) (count! branched (caddr t)) (count! branched (cadddr t)))
        ((throw)
         (count! throws (cadr t))
         (hashtable-update! thrown (cadr t) (lambda (l) (cons (cddr t) l)) '()))
        ((call) (count! called (cadr t)))
        ((app) (count! called (cadddr t)))
        ((args) (walk (caddr t)))
        (else (void)))))


  ;; The objects of t only written into: each alloc's variable and its casts (as-val r), to #t.
  ;; Optimistic: an object stored into one is dead unless that one is not.
  (define (dead-objects t)
    (let ((aliases (make-eq-hashtable))
          (live (make-eq-hashtable))
          (into (make-eq-hashtable)))
      (define (object a)
        (cond
         ((symbol? a) (hashtable-ref aliases a #f))
         ((and (pair? a) (memq (car a) '(as-raw as-val)) (pair? (cdr a))) (object (cadr a)))
         (else #f)))

      (define (use! a)
        (cond
         ((object a) => (lambda (r) (hashtable-set! live r #t)))
         ((pair? a) (for-each use! (cdr a)))))

      (let walk ((t t))
        (case (car t)
          ((letprim)
           (let ((x (cadr t))
                 (op (caddr t)))
             (case (car op)
               ((alloc)
                (for-each use! (cdr op))
                (when x
                  (hashtable-set! aliases x x)))
               ((as-val)
                (let ((r (object (cadr op))))
                  (if (and x r (symbol? (cadr op)))
                      (hashtable-set! aliases x r)
                      (use! (cadr op)))))
               ((store copy-words)
                (let ((r (object (cadr op))))
                  (if r
                      (for-each (lambda (a)
                                  (let ((o (object a)))
                                    (if (and o (symbol? a))
                                        (hashtable-update! into o (lambda (l) (cons r l)) '())
                                        (use! a))))
                                (cddr op))
                      (for-each use! (cdr op)))))
               (else (for-each use! (op-atoms op)))))
           (walk (cadddr t)))
          ((letcont letcont-rec)
           (walk (caddr (car (cadr t))))
           (walk (caddr t)))
          ((args) (walk (caddr t)))
          ((if) (for-each use! (cdadr t)))
          ((throw) (for-each use! (cddr t)))
          ((call) (use! (cadddr t)))
          ((app) (use! (caddr t)) (for-each use! (cddddr t)))
          ((selfcall) (for-each use! (cdr t)))
          (else (void))))
      ;; Stored into a live object, live
      (let loop ()
        (let ((more? #f))
          (vector-for-each
           (lambda (o)
             (when (and (not (hashtable-ref live o #f))
                        (exists (lambda (r) (hashtable-ref live r #f)) (hashtable-ref into o '())))
               (hashtable-set! live o #t)
               (set! more? #t)))
           (hashtable-keys into))
          (when more? (loop))))
      (let ((dead (make-eq-hashtable)))
        (vector-for-each
         (lambda (x)
           (unless (hashtable-ref live (hashtable-ref aliases x #f) #f)
             (hashtable-set! dead x #t)))
         (hashtable-keys aliases))
        dead)))


  (define (simplify-round def fun)
    (let* ((own (car (caddr fun)))
           (census (census (fun-term fun)))
           (dead (dead-objects (fun-term fun)))
           (throws (make-eq-hashtable))
           (thrown (make-eq-hashtable))
           (positions (make-eq-hashtable))
           (spans (make-eq-hashtable))
           ;; The continuations that lost parameters, to the indexes of those kept
           (kept (make-eq-hashtable))
           (branched (make-eq-hashtable))
           (called (make-eq-hashtable))
           (cells (make-eq-hashtable))
           ;; Variables to atoms, continuations to continuations
           (subst (make-eq-hashtable))
           ;; The continuations substituted at their throw, to (params . body)
           (inlined (make-eq-hashtable))
           ;; The continuations whose body is an if on their parameters, to (params . test)
           (threaded (make-eq-hashtable))
           (changed? #f))
      (define (changed!)
        (set! changed? #t))

      (define (uses x)
        (use-count census x))

      (define (cont k)
        (let ((j (hashtable-ref subst k #f)))
          (if j (cont j) k)))

      (define (atom a)
        (cond
         ((symbol? a) (hashtable-ref subst a a))
         ((pair? a) (simplify-cast (cons (car a) (map atom (cdr a)))))
         (else a)))

      ;; x replaced by the atom a, unless either is a cell
      (define (substitute! x a)
        (and (not (hashtable-ref cells x #f))
             (not (and (symbol? a) (hashtable-ref cells a #f)))
             (begin
               (hashtable-set! subst x a)
               (changed!)
               #t)))

      (define (walk t)
        (case (car t)
          ((letprim) (walk-letprim (cadr t) (map-op atom (caddr t)) (cadddr t)))
          ((letcont) (walk-letcont t))
          ((letcont-rec)
           (let ((b (car (cadr t))))
             (if (zero? (uses (car b)))
                 (begin
                   (changed!)
                   (walk (caddr t)))
                 (let* ((params (invariant-params! (car b) (cadr b)))
                        (main (walk (caddr t))))
                   `(letcont-rec ((,(car b) ,params ,(walk (caddr b)))) ,main)))))
          ((if) (walk-if (cons (caadr t) (map atom (cdadr t))) (cont (caddr t)) (cont (cadddr t))))
          ((throw) (walk-throw (cont (cadr t)) (map atom (cddr t))))
          ((call) `(call ,(cont (cadr t)) ,(caddr t) ,(atom (cadddr t)) ,(list-ref t 4)))
          ((app) `(app ,(cadr t) ,(atom (caddr t)) ,(cont (cadddr t)) ,@(map atom (cddddr t))))
          ((selfcall) `(selfcall ,@(map atom (cdr t))))
          ((args) `(args ,(cadr t) ,(walk (caddr t))))
          (else t)))

      (define (dead-object? a)
        (cond
         ((symbol? a) (hashtable-ref dead a #f))
         ((and (pair? a) (memq (car a) '(as-raw as-val)) (pair? (cdr a))) (dead-object? (cadr a)))
         (else #f)))

      (define (walk-letprim x op body)
        (cond
         ((and (or (not x) (zero? (uses x))) (memq (cps-effect op) '(pure reads)))
          (changed!)
          (walk body))
         ((or (and x (memq (car op) '(alloc as-val)) (hashtable-ref dead x #f))
              (and (memq (car op) '(store copy-words)) (dead-object? (cadr op))))
          (changed!)
          (walk body))
         ((and x (fold op)) => (lambda (a)
                                 (if (substitute! x a)
                                     (walk body)
                                     `(letprim ,x ,op ,(walk body)))))
         (else `(letprim ,x ,op ,(walk body)))))

      (define (walk-letcont t)
        (let* ((b (car (cadr t)))
               (k (car b))
               (params (cadr b))
               (body (caddr b))
               (vars (map param-var params)))
          (cond
           ((zero? (uses k))
            (changed!)
            (walk (caddr t)))
           ((= (hashtable-ref throws k 0) (uses k) 1)
            (hashtable-set! inlined k (cons vars body))
            (changed!)
            (walk (caddr t)))
           ((eta-target k vars body)
            => (lambda (j)
                 (hashtable-set! subst k j)
                 (changed!)
                 (walk (caddr t))))
           ((floated k params body (caddr t))
            => (lambda (t)
                 (changed!)
                 (walk t)))
           (else
            (let* ((params (invariant-params! k params))
                   (vars (map param-var params)))
              (when (tests-only? vars body)
                (hashtable-set! threaded k (cons vars body)))
              (let ((main (walk (caddr t))))
                `(letcont ((,k ,params ,(walk body))) ,main)))))))

      ;; The parameters of k but the invariant ones, each substituted by its atom
      (define (invariant-params! k params)
        (if (or (positive? (hashtable-ref branched k 0)) (positive? (hashtable-ref called k 0)))
            params
            (let* ((span (hashtable-ref spans k #f))
                   (throws (hashtable-ref thrown k '()))
                   (indexes
                    (filter (lambda (i)
                              (let* ((p (param-var (list-ref params i)))
                                     (as (filter (lambda (a) (not (eq? a p)))
                                                 (map (lambda (as) (list-ref as i)) throws))))
                                (not (and (pair? as)
                                          (for-all (lambda (a) (equal? a (car as))) as)
                                          (outside? (car as) span)
                                          (substitute! p (atom (car as)))))))
                            (iota (length params)))))
              (if (= (length indexes) (length params))
                  params
                  (begin
                    (hashtable-set! kept k indexes)
                    (map (lambda (i) (list-ref params i)) indexes))))))

      ;; Whether the variables of the atom a are bound outside the span
      (define (outside? a span)
        (cond
         ((symbol? a)
          (let ((p (hashtable-ref positions a #f)))
            (or (not p) (< p (car span)) (>= p (cdr span)))))
         ((pair? a) (for-all (lambda (a) (outside? a span)) (cdr a)))
         (else #t)))

      ;; The continuation k is, when its body throws its parameters on: not one substituted at
      ;; its throw; the return continuation only for throws, or for calls of a fun returning a
      ;; val, which become tail calls
      (define (eta-target k vars body)
        (and (eq? (car body) 'throw)
             (equal? (cddr body) vars)
             (let ((j (cont (cadr body))))
               (and (not (hashtable-ref inlined j #f))
                    (or (not (eq? j own))
                        (and (zero? (hashtable-ref branched k 0))
                             (or (zero? (hashtable-ref called k 0))
                                 (eq? (def-result def) 'val))))
                    j))))

      ;; An if on the parameters vars and constants
      (define (tests-only? vars t)
        (and (eq? (car t) 'if)
             (for-all (lambda (a) (or (memq a vars) (constant? a))) (map atom (cdadr t)))))

      ;; (letcont ((k params body)) main) with the letconts heading body moved before k's, when
      ;; body then is an if on k's parameters, so the throws to k can be threaded; or #f
      (define (floated k params body main)
        (let ((vars (map param-var params)))
          (let loop ((t body) (heads '()))
            (cond
             ((and (eq? (car t) 'letcont)
                   (not (mentions? (caddr (car (cadr t))) vars)))
              (loop (caddr t) (cons (car (cadr t)) heads)))
             ((and (pair? heads) (tests-only? vars t))
              (fold-left (lambda (main b) `(letcont (,b) ,main))
                         `(letcont ((,k ,params ,t)) ,main)
                         heads))
             (else #f)))))

      (define (walk-if test k1 k2)
        (let ((known (known-test test)))
          (cond
           (known
            (changed!)
            `(throw ,(if (eq? known 'true) k1 k2)))
           ((eq? k1 k2)
            (changed!)
            `(throw ,k1))
           (else `(if ,test ,k1 ,k2)))))

      (define (walk-throw k as)
        (let ((as (cond
                   ((hashtable-ref kept k #f) => (lambda (is) (map (lambda (i) (list-ref as i)) is)))
                   (else as))))
          (walk-throw-kept k as)))

      (define (walk-throw-kept k as)
        (cond
         ((hashtable-ref inlined k #f)
          => (lambda (entry)
               (for-each (lambda (x a)
                           (unless (substitute! x a)
                             (compile-error 'simplify "cell thrown to a continuation" x)))
                         (car entry) as)
               (walk (cdr entry))))
         ((hashtable-ref threaded k #f)
          => (lambda (entry)
               (let* ((thrown (map cons (car entry) as))
                      (body (cdr entry))
                      (test (cadr body))
                      (known (known-test
                              (cons (car test)
                                    (map (lambda (a)
                                           (cond ((assq a thrown) => cdr) (else (atom a))))
                                         (cdr test))))))
                 (if known
                     (begin
                       (changed!)
                       `(throw ,(cont (if (eq? known 'true) (caddr body) (cadddr body)))))
                     `(throw ,k ,@as)))))
         (else `(throw ,k ,@as))))

      (let ((term (begin
                    (count-cont-uses! (fun-term fun) throws thrown branched called cells positions
                                      spans)
                    (walk (fun-term fun)))))
        (values `(,@(list-head fun 3) ,term) changed?))))


  ;; Whether the term t uses one of the variables vars
  (define (mentions? t vars)
    (define (atom? a)
      (cond
       ((symbol? a) (memq a vars))
       ((pair? a) (exists atom? (cdr a)))
       (else #f)))

    (case (car t)
      ((letprim) (or (exists atom? (op-atoms (caddr t))) (mentions? (cadddr t) vars)))
      ((letcont letcont-rec) (or (mentions? (caddr (car (cadr t))) vars) (mentions? (caddr t) vars)))
      ((if) (exists atom? (cdadr t)))
      ((throw) (exists atom? (cddr t)))
      ((call) (atom? (cadddr t)))
      ((app) (or (atom? (caddr t)) (exists atom? (cddddr t))))
      ((selfcall) (exists atom? (cdr t)))
      ((args) (mentions? (caddr t) vars))
      (else #f)))


  ;; Atoms

  (define two^63 (expt 2 63))
  (define two^64 (expt 2 64))


  (define (signed n)
    (let ((m (modulo n two^64)))
      (if (>= m two^63) (- m two^64) m)))


  (define (unsigned n)
    (modulo n two^64))


  (define (quad? a)
    (and (pair? a) (eq? (car a) 'quad)))


  (define (quad n)
    `(quad ,(signed n)))


  (define (word a)
    (signed (cadr a)))


  ;; A val constant, (as-val (quad n)) or (as-val "symbol")
  (define (val-constant? a)
    (and (pair? a) (eq? (car a) 'as-val) (or (quad? (cadr a)) (string? (cadr a)))))


  (define (constant? a)
    (or (quad? a) (string? a) (val-constant? a)))


  ;; A cast of a cast, or of a constant
  (define (simplify-cast a)
    (case (car a)
      ((as-raw)
       (let ((x (cadr a)))
         (cond
          ((and (pair? x) (eq? (car x) 'as-val)) (cadr x))
          ((or (quad? x) (string? x)) x)
          (else a))))
      ((as-val)
       (let ((x (cadr a)))
         (if (and (pair? x) (eq? (car x) 'as-raw)) (cadr x) a)))
      (else a)))


  ;; The atom op's value is, or #f
  (define (fold op)
    (case (car op)
      ((as-val)
       (let ((a (cadr op)))
         (cond
          ((or (quad? a) (string? a)) `(as-val ,a))
          ((and (pair? a) (eq? (car a) 'as-raw)) (cadr a))
          (else #f))))
      ((stackcall move cell-set!) #f)
      (else
       (let ((args (cdr op)))
         (and (= (length args) 2)
              (if (and (quad? (car args)) (quad? (cadr args)))
                  (let ((n (evaluate (car op) (word (car args)) (word (cadr args)))))
                    (and n (quad n)))
                  (algebra (car op) (car args) (cadr args))))))))


  ;; a op b on words, or #f
  (define (evaluate name a b)
    (case name
      ((bit-and) (bitwise-and a b))
      ((bit-or) (bitwise-ior a b))
      ((bit-xor) (bitwise-xor a b))
      ((raw+) (+ a b))
      ((raw-) (- a b))
      ((raw*) (* a b))
      ((raw/) (and (not (zero? b)) (not (and (= a (- two^63)) (= b -1))) (quotient a b)))
      ((raw%) (and (not (zero? b)) (not (and (= a (- two^63)) (= b -1))) (remainder a b)))
      ((raw*hu) (quotient (* (unsigned a) (unsigned b)) two^64))
      ((raw<<) (and (<= 0 b 63) (bitwise-arithmetic-shift a b)))
      ((raw>>) (and (<= 0 b 63) (bitwise-arithmetic-shift a (- b))))
      ((raw>>u) (and (<= 0 b 63) (bitwise-arithmetic-shift (unsigned a) (- b))))
      (else #f)))


  ;; a op b when an operand is a unit or a zero, or #f
  (define (algebra name a b)
    (define (is? x n)
      (and (quad? x) (= (word x) n)))

    (case name
      ((raw+ bit-or bit-xor)
       (cond ((is? b 0) a) ((is? a 0) b) (else #f)))
      ((raw- raw<< raw>> raw>>u)
       (and (is? b 0) a))
      ((bit-and)
       (cond ((is? b -1) a) ((is? a -1) b) ((or (is? a 0) (is? b 0)) '(quad 0)) (else #f)))
      ((raw*)
       (cond ((is? b 1) a) ((is? a 1) b) ((or (is? a 0) (is? b 0)) '(quad 0)) (else #f)))
      (else #f)))


  ;; true or false when the test's outcome is known, else #f
  (define (known-test test)
    (define (answer b)
      (if b 'true 'false))

    (let ((name (car test))
          (as (cdr test)))
      (case name
        ((true)
         (let ((a (car as)))
           (and (val-constant? a)
                (answer (not (equal? a (immediate #f)))))))
        ((eq)
         (cond
          ((equal? (car as) (cadr as)) 'true)
          ((and (val-constant? (car as)) (val-constant? (cadr as))
                (quad? (cadar as)) (quad? (cadadr as)))
           (answer (= (word (cadar as)) (word (cadadr as)))))
          (else #f)))
        (else
         (let ((a (car as))
               (b (cadr as)))
           (cond
            ((and (quad? a) (quad? b))
             (answer (compare name (word a) (word b))))
            ((equal? a b)
             (answer (compare name 0 0)))
            (else #f)))))))


  (define (compare name a b)
    (case name
      ((raw=) (= a b))
      ((raw<) (< a b))
      ((raw<=) (<= a b))
      ((raw>) (> a b))
      ((raw>=) (>= a b))
      ((raw<u) (< (unsigned a) (unsigned b)))
      ((raw<=u) (<= (unsigned a) (unsigned b)))
      ((raw>u) (> (unsigned a) (unsigned b)))
      ((raw>=u) (>= (unsigned a) (unsigned b)))
      (else (compile-error 'simplify "unknown test" name)))))
