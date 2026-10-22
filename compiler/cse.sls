;; Common subexpressions and facts: a letprim whose op, its atoms substituted, equals one in scope
;; is that one's variable, and an if whose test is decided by the tests dominating it goes to its
;; branch. One walk down the term, scope being dominance: each letcont is moved down to the
;; innermost term enclosing all its uses, so the letprims dominating its body enclose it.
;; - a pure op is shared while in scope, but a literal: reloading it is cheaper than keeping it
;; - a reading op only with no write, app or call between
;; - a raw result only with no safepoint between (the GC rule of cps-effect)
;; Writes and safepoints start new epochs, path by path; a continuation's body starts in the
;; epochs of its uses where they all agree, else in new ones (a return point always).
;; Cells, targets of a cell-set!, are never shared nor in a shared op.
;;
;; Facts: the body of a continuation used once, as a branch of an if, knows the test's outcome.
;; A test is keyed by its canonical form: variables replaced by the ops binding them, the tests
;; normalized to raw=, raw< or raw<u. A raw= with a constant also gives the value of the other
;; side, so a tag known as 1 decides the other tag tests.
;; A fact holds in the memory epoch where it was found when it reads memory, and in its
;; safepoint epoch unless the collector cannot change it: tags, headers, eq, words of fixnums.

(library (cse)
  (export cse)
  (import (chezscheme)
          (utils)
          (output)
          (cps)
          (simplify))


  ;; A letcont not placed yet: its body with the pending letconts used only there
  (define-record-type pending
    (fields k params body inner))


  ;; The largest canonical form kept for a variable
  (define max-form-size 16)


  (define (cse def fun)
    (let ((free (make-eq-hashtable))
          (cells (make-eq-hashtable))
          (kinds (cps-kinds fun))
          (census (census (fun-term fun)))
          ;; Variables to variables
          (subst (make-eq-hashtable))
          ;; Ops to (x mem gc), mem and gc the epochs the entry needs, or #f
          (table (make-hashtable equal-hash equal?))
          ;; Continuations to the epochs (mem . gc) of their uses, each #f when they differ
          (uses (make-eq-hashtable))
          ;; Raw and val variables to (form . mem): their canonical form, valid in mem or always
          (forms (make-eq-hashtable))
          ;; Normalized tests to (outcome mem gc), (value form) to (n mem gc)
          (facts (make-hashtable equal-hash equal?))
          ;; Continuations used once, as a branch, to the facts their body starts with
          (branches (make-eq-hashtable))
          (next (make-counter)))
      (define (atom a)
        (cond
         ((symbol? a) (hashtable-ref subst a a))
         ((pair? a) (cons (car a) (map atom (cdr a))))
         (else a)))

      (define (cell-atom? a)
        (cond
         ((symbol? a) (hashtable-ref cells a #f))
         ((pair? a) (exists cell-atom? (cdr a)))
         (else #f)))

      (define (use! k mem gc)
        (let ((u (hashtable-ref uses k #f)))
          (hashtable-set! uses k (if u
                                     (cons (and (eqv? (car u) mem) mem) (and (eqv? (cdr u) gc) gc))
                                     (cons mem gc)))))

      (define (valid? e mem gc)
        (and (or (not (cadr e)) (eqv? (cadr e) mem))
             (or (not (caddr e)) (eqv? (caddr e) gc))))

      (define (lookup op mem gc)
        (let ((e (hashtable-ref table op #f)))
          (and e (valid? e mem gc) (car e))))

      ;; Canonical forms

      (define (canon a mem)
        (cond
         ((symbol? a)
          (let* ((x (hashtable-ref subst a a))
                 (f (hashtable-ref forms x #f)))
            (if (and f (or (not (cdr f)) (eqv? (cdr f) mem))) (car f) x)))
         ((not (pair? a)) a)
         (else
          (case (car a)
            ((quad self) a)
            ((as-raw)
             (let ((x (canon (cadr a) mem)))
               (cond
                ((and (pair? x) (eq? (car x) 'as-val)) (cadr x))
                ((or (string? x) (quad? x)) x)
                (else `(as-raw ,x)))))
            ((as-val)
             (let ((x (canon (cadr a) mem)))
               (if (and (pair? x) (eq? (car x) 'as-raw)) (cadr x) `(as-val ,x))))
            (else (cons (car a) (map (lambda (a) (canon a mem)) (cdr a))))))))

      (define (form! x op mem)
        (let ((f (canon op mem)))
          (when (<= (canon-size f) max-form-size)
            (hashtable-set! forms x (cons f (and (reads-form? f) mem))))))

      ;; Facts

      (define (fact key mem gc)
        (let ((e (hashtable-ref facts key #f)))
          (and e (valid? e mem gc) e)))

      (define (value-of f mem gc)
        (cond
         ((quad? f) (word f))
         ((fact `(value ,f) mem gc) => car)
         ((tag-mask f)
          => (lambda (m)
               (exists (lambda (m2)
                         (and (= (bitwise-and m m2) m)
                              (let ((e (fact `(value (bit-and ,(cadr f) (quad ,m2))) mem gc)))
                                (and e (bitwise-and (car e) m)))))
                       tag-masks)))
         (else #f)))

      ;; (bit-and x (quad m)) = c is false when a known tag of x disagrees on the bits of m
      (define (tag-differs? a b mem gc)
        (let ((m (tag-mask a)))
          (and m
               (quad? b)
               (exists (lambda (m2)
                         (let ((v (value-of `(bit-and ,(cadr a) (quad ,m2)) mem gc))
                               (both (bitwise-and m m2)))
                           (and v (not (= (bitwise-and v both) (bitwise-and (word b) both))))))
                       tag-masks))))

      ;; x a val known to be an immediate, from its tag
      (define (immediate? x mem gc)
        (memv (value-of `(bit-and (as-raw ,x) (quad 7)) mem gc) '(1 2)))

      ;; The collector cannot change f's value: it moves objects, changing their addresses
      ;; but not their tags or headers, so a raw variable (live, so not across a safepoint), a
      ;; header, the tag of a word or a fixnum's word
      (define (stable? f mem gc)
        (cond
         ((symbol? f) #t)
         ((not (pair? f)) #t)
         (else
          (case (car f)
            ((quad) #t)
            ((self) #f)
            ((as-raw) (immediate? (cadr f) mem gc))
            ((load load8 load32) (header-form? f))
            (else
             (or (and (tag-mask f) (word-form? (cadr f)))
                 (for-all (lambda (a) (stable? a mem gc)) (cdr f))))))))

      ;; A word compared for equality: an address only ever equals the same object's
      (define (test-stable? op a b mem gc)
        (define (side? f)
          (or (word-form? f) (stable? f mem gc)))
        (if (eq? op 'raw=)
            (and (side? a) (side? b))
            (and (stable? a mem gc) (stable? b mem gc))))

      ;; #t or #f when the facts decide the normalized test, else 'unknown
      (define (decide op a b mem gc)
        (cond
         ((or (fact (list op a b) mem gc)
              (and (eq? op 'raw=) (fact (list op b a) mem gc)))
          => car)
         ((and (value-of a mem gc) (value-of b mem gc))
          (compare op (value-of a mem gc) (value-of b mem gc)))
         ((and (eq? op 'raw=) (tag-differs? a b mem gc)) #f)
         (else 'unknown)))

      ;; The entries the outcome of the normalized test adds, as (key value mem gc)
      (define (outcome-facts op a b outcome mem gc)
        (let ((mem (and (or (reads-form? a) (reads-form? b)) mem))
              (gc (and (not (test-stable? op a b mem gc)) gc)))
          (cons (list (list op a b) outcome mem gc)
                (if (and outcome (eq? op 'raw=) (quad? b))
                    (list (list `(value ,a) (word b) mem gc))
                    '()))))

      (define (with-facts entries thunk)
        (if (null? entries)
            (thunk)
            (with-entry facts (car (car entries)) (cdr (car entries))
                        (lambda () (with-facts (cdr entries) thunk)))))

      ;; The term

      (define (walk t pending mem gc)
        (case (car t)
          ((letprim) (walk-letprim (cadr t) (map-op atom (caddr t)) (cadddr t) pending mem gc))
          ((letcont letcont-rec) (walk-letcont t pending mem gc))
          ;; The frame is allocated at the mark
          ((args) `(args ,(cadr t) ,(walk (caddr t) pending mem (next))))
          (else (place pending (lambda () (leaf t mem gc))))))

      (define (walk-letprim x op body pending mem gc)
        (let* ((effect (cps-effect op))
               (shared? (and x
                             (memq effect '(pure reads))
                             (not (eq? (car op) 'literal))
                             (not (hashtable-ref cells x #f))
                             (not (exists cell-atom? (op-atoms op))))))
          (cond
           ((and shared? (lookup op mem gc))
            => (lambda (y)
                 (hashtable-set! subst x y)
                 (walk body pending mem gc)))
           (else
            (when (and shared? (not (memq (car op) '(stackcall move))))
              (form! x op mem))
            (let* ((mem (if (eq? effect 'writes) (next) mem))
                   (gc (if (eq? effect 'allocates) (next) gc))
                   (walk-body (lambda () (walk body pending mem gc))))
              `(letprim ,x ,op
                        ,(if shared?
                             (with-entry table op
                                         (list x
                                               (and (eq? effect 'reads) mem)
                                               (and (eq? (hashtable-ref kinds x #f) 'raw) gc))
                                         walk-body)
                             (walk-body))))))))

      ;; The pending letconts used in both k's body and the main term are placed here, those
      ;; used in one go with it; k is pending in the main term (a letcont-rec stays)
      (define (walk-letcont t pending mem gc)
        (let* ((b (car (cadr t)))
               (k (car b))
               (params (cadr b))
               (body (caddr b))
               (main (caddr t))
               (fs (hashtable-ref free t #f)))
          (let loop ((ps pending) (here '()) (to-body '()) (to-main '()))
            (if (pair? ps)
                (let* ((j (pending-k (car ps)))
                       (in-body? (memq j (car fs)))
                       (in-main? (memq j (cdr fs))))
                  (cond
                   ((and in-body? (not in-main?)) (loop (cdr ps) here (cons (car ps) to-body) to-main))
                   ((and in-main? (not in-body?)) (loop (cdr ps) here to-body (cons (car ps) to-main)))
                   (else (loop (cdr ps) (cons (car ps) here) to-body to-main))))
                (place here
                       (lambda ()
                         (cond
                          ((eq? (car t) 'letcont-rec)
                           (let ((main (walk main to-main mem gc)))
                             `(letcont-rec ((,k ,params ,(walk body to-body (next) (next)))) ,main)))
                          ((memq k (cdr fs))
                           (walk main (cons (make-pending k params body to-body) to-main) mem gc))
                          (else
                           (place (list (make-pending k params body to-body))
                                  (lambda () (walk main to-main mem gc)))))))))))

      ;; (make-term) with the letconts of pending around it, their bodies walked after it, in
      ;; the epochs of their uses and with the facts of their branch; one left unused by a
      ;; decided if is dropped
      (define (place pending make-term)
        (let ((t (make-term)))
          (fold-left (lambda (t p)
                       (let ((u (hashtable-ref uses (pending-k p) #f)))
                         (if u
                             `(letcont ((,(pending-k p) ,(pending-params p)
                                         ,(with-facts (hashtable-ref branches (pending-k p) '())
                                                      (lambda ()
                                                        (walk (pending-body p) (pending-inner p)
                                                              (or (car u) (next)) (or (cdr u) (next)))))))
                                       ,t)
                             t)))
                     t
                     pending)))

      (define (leaf t mem gc)
        (case (car t)
          ((if) (leaf-if (cons (caadr t) (map atom (cdadr t))) (caddr t) (cadddr t) mem gc))
          ((throw)
           (use! (cadr t) mem gc)
           `(throw ,(cadr t) ,@(map atom (cddr t))))
          ((call)
           (use! (cadr t) (next) (next))
           `(call ,(cadr t) ,(caddr t) ,(atom (cadddr t))))
          ((app)
           (use! (cadddr t) (next) (next))
           `(app ,(cadr t) ,(atom (caddr t)) ,(cadddr t) ,@(map atom (cddddr t))))
          ((selfcall) `(selfcall ,@(map atom (cdr t))))
          (else t)))

      (define (leaf-if test k1 k2 mem gc)
        (let* ((n (and (not (exists cell-atom? (cdr test))) (normalize-test test (lambda (a) (canon a mem)))))
               (outcome (if n (decide (car n) (cadr n) (caddr n) mem gc) 'unknown)))
          (define (branch! k taken?)
            (when (and n (not (eq? k1 k2)) (= (use-count census k) 1))
              (hashtable-set! branches k
                              (outcome-facts (car n) (cadr n) (caddr n)
                                             (eq? taken? (cadddr n)) mem gc))))

          (if (eq? outcome 'unknown)
              (begin
                (use! k1 mem gc)
                (use! k2 mem gc)
                (branch! k1 #t)
                (branch! k2 #f)
                `(if ,test ,k1 ,k2))
              (let ((k (if (eq? outcome (cadddr n)) k1 k2)))
                (use! k mem gc)
                `(throw ,k)))))

      (free-conts! (fun-term fun) free cells)
      `(,@(list-head fun 3) ,(walk (fun-term fun) '() 0 0))))


  ;; The masks of tag tests, of which a known one decides the others
  (define tag-masks '(7 3 1))


  ;; m when f is (bit-and x (quad m)), m a mask of tag bits, else #f
  (define (tag-mask f)
    (and (pair? f)
         (eq? (car f) 'bit-and)
         (quad? (caddr f))
         (<= 0 (word (caddr f)) 7)
         (word (caddr f))))


  (define (load-form? f)
    (and (pair? f) (memq (car f) '(load load8 load32))))


  ;; A header: the word 0 of a val's address, which only an object with one has
  (define (header-form? f)
    (and (eq? (car f) 'load)
         (pair? (cadr f))
         (eq? (car (cadr f)) 'as-raw)
         (equal? (caddr f) '(quad 0))))


  ;; A val's word, or one loaded
  (define (word-form? f)
    (and (pair? f) (or (eq? (car f) 'as-raw) (load-form? f))))


  (define (reads-form? f)
    (and (pair? f) (or (load-form? f) (exists reads-form? (cdr f)))))


  (define (canon-size f)
    (if (pair? f) (fold-left + 1 (map canon-size (cdr f))) 1))


  ;; The test as (op a b positive?), op raw=, raw< or raw<u, its atoms canonical: the test is
  ;; (op a b) when positive?, else its negation
  (define (normalize-test test canon)
    (let ((name (car test))
          (args (map canon (cdr test))))
      (define (raw a)
        (canon `(as-raw ,a)))

      (define (equal-test a b)
        (if (quad? a) (list 'raw= b a #t) (list 'raw= a b #t)))

      (case name
        ((eq) (equal-test (raw (cadr test)) (raw (caddr test))))
        ((true) (list 'raw= (raw (cadr test)) (cadr (immediate #f)) #f))
        ((raw=) (equal-test (car args) (cadr args)))
        ((raw<) (list 'raw< (car args) (cadr args) #t))
        ((raw>=) (list 'raw< (car args) (cadr args) #f))
        ((raw>) (list 'raw< (cadr args) (car args) #t))
        ((raw<=) (list 'raw< (cadr args) (car args) #f))
        ((raw<u) (list 'raw<u (car args) (cadr args) #t))
        ((raw>=u) (list 'raw<u (car args) (cadr args) #f))
        ((raw>u) (list 'raw<u (cadr args) (car args) #t))
        ((raw<=u) (list 'raw<u (cadr args) (car args) #f))
        (else #f))))


  ;; The continuations each subterm uses: for each letcont, (body-conts . main-conts) in free;
  ;; the targets of cell-set! in cells
  (define (free-conts! t free cells)
    (let walk ((t t))
      (case (car t)
        ((letprim)
         (when (eq? (car (caddr t)) 'cell-set!)
           (hashtable-set! cells (cadr (caddr t)) #t))
         (walk (cadddr t)))
        ((letcont letcont-rec)
         (let* ((b (car (cadr t)))
                (fb (walk (caddr b)))
                (fm (walk (caddr t))))
           (hashtable-set! free t (cons fb fm))
           (remq (car b) (fold-left (lambda (ks k) (adjoin k ks)) fb fm))))
        ((args) (walk (caddr t)))
        ((if) (list (caddr t) (cadddr t)))
        ((throw call) (list (cadr t)))
        ((app) (list (cadddr t)))
        (else '())))))
