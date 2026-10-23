;; Lifting known closures, on the funs of a unit (lambda lifting): closures created once each, in
;; a fun g, whose values are only called there or captured by each other, become defs of no
;; captures, together. Each takes the group's other captures as extra parameters: a call
;; (app tag v k atom ...) through a value of the group a direct call of its def with the
;; captured atoms after the arguments, a capture load in a body that parameter, a call through a
;; capture holding a closure of the group a direct call passing the parameters on. The
;; allocations are left unused, for the simplifier.
;; A closure is (alloc), (copy-words r "x_proc" (quad 5)), then a store of each capture i at
;; word i + 5, of r or of its value (as-val r); its body loads capture i from (self).

(library (lift)
  (export lift-unit)
  (import (chezscheme)
          (utils)
          (defs)
          (cps))


  ;; A closure's creation: its def, its object and value, the index of the fun creating it, its
  ;; captured atoms by word ((word . atom) ..., the last stored first), the arities of its calls
  ;; and the objects' stores seen at each, the objects its value is stored into, and whether
  ;; something else uses its value
  (define-record-type creation
    (fields def object value fun (mutable slots) (mutable calls) (mutable call-states)
            (mutable into) (mutable escapes?)))


  ;; defs and funs in the same order; (reoptimize def fun) optimizes a changed fun
  ;; again. Gives the funs.
  (define (lift-unit defs funs reoptimize)
    (let ((procs (make-hashtable string-hash string=?))
          (index (make-eq-hashtable))
          (creations (make-eq-hashtable))
          (by-object (make-eq-hashtable))
          (funs (list->vector funs)))
      (for-each (lambda (def i)
                  (hashtable-set! procs (def-proc def) def)
                  (hashtable-set! index def i))
                defs (iota (length defs)))
      (for-each (lambda (g)
                  (let ((fun (vector-ref funs g)))
                    (when fun
                      (find-creations! fun g procs creations by-object))))
                (iota (vector-length funs)))
      (let* ((candidates (filter (lambda (c) c)
                                 (map (lambda (def)
                                        (let ((cs (hashtable-ref creations def '())))
                                          (and (= (length cs) 1)
                                               (candidate? (car cs) (hashtable-ref index def #f)
                                                           by-object funs)
                                               (car cs))))
                                      defs)))
             (members (lifted-members candidates index by-object funs)))
        (if (null? members)
            (vector->list funs)
            (let ((subst (make-eq-hashtable))
                  (dropped (make-eq-hashtable))
                  (callees (make-eq-hashtable))
                  (new-params (make-eq-hashtable))
                  (changed (make-eqv-hashtable)))
              (for-each (lambda (group)
                          (lift-group! group index funs by-object subst dropped callees
                                       new-params)
                          (for-each (lambda (c)
                                      (hashtable-set! changed (hashtable-ref index (creation-def c) #f) #t)
                                      (hashtable-set! changed (creation-fun c) #t))
                                    group))
                        (filter (lambda (group)
                                  (for-all (lambda (c)
                                             (for-all (lambda (state)
                                                        (for-all (lambda (m) (complete? m state)) group))
                                                      (creation-call-states c)))
                                           group))
                                (groups members by-object)))
              (for-each (lambda (i)
                          (let ((def (list-ref defs i)))
                            (vector-set! funs i
                                         (reoptimize def (lift-rewrite (vector-ref funs i) def subst
                                                                       dropped callees new-params)))))
                        (sort < (vector->list (hashtable-keys changed))))
              (vector->list funs))))))


  ;; The object an atom names: the variable r of an alloc, through (as-val r) and casts
  (define (object-of a aliases)
    (cond
     ((symbol? a) (hashtable-ref aliases a #f))
     ((and (pair? a) (memq (car a) '(as-raw as-val)) (pair? (cdr a))) (object-of (cadr a) aliases))
     (else #f)))


  ;; Records the closures fun g creates, in creations (def to creations) and by-object (the
  ;; object r of each, to its creation)
  (define (find-creations! fun g procs creations by-object)
    (let ((aliases (make-eq-hashtable))
          (procs-of (make-eq-hashtable))
          (census (census (fun-term fun)))
          (allowed (make-eq-hashtable)))
      (define (creation-of a)
        (let ((r (object-of a aliases)))
          (and r (hashtable-ref by-object r #f))))

      (define (allow! v)
        (hashtable-update! allowed v (lambda (n) (+ n 1)) 0))

      ;; objects: per allocated r, the stores seen on this path, ((r (word . atom) ...) ...)
      (define (walk t objects)
        (case (car t)
          ((letprim)
           (let ((x (cadr t))
                 (op (caddr t)))
             (case (car op)
               ((alloc)
                (when x
                  (hashtable-set! aliases x x))
                (walk (cadddr t) (if x (cons (list x) objects) objects)))
               ((copy-words)
                (let ((r (object-of (cadr op) aliases)))
                  (when (and r (string? (caddr op)))
                    (hashtable-set! procs-of r (caddr op))))
                (walk (cadddr t) objects))
               ((as-val)
                (let ((r (object-of (cadr op) aliases)))
                  (when (and x r)
                    (hashtable-set! aliases x r)
                    (let ((def (hashtable-ref procs (hashtable-ref procs-of r "") #f)))
                      (when (and def (not (hashtable-ref by-object r #f)))
                        (let ((c (make-creation def r x g (cdr (or (assq r objects) (list r))) '()
                                                '() '() #f)))
                          (hashtable-set! by-object r c)
                          (hashtable-update! creations def (lambda (l) (cons c l)) '()))))))
                (walk (cadddr t) objects))
               ((store)
                (let ((r (object-of (cadr op) aliases))
                      (word (caddr op))
                      (value (cadddr op)))
                  (let ((c (and r (hashtable-ref by-object r #f))))
                    (when (and c (equal? (cadr op) `(as-raw ,(creation-value c))))
                      (allow! (creation-value c))))
                  (let ((stored (creation-of value)))
                    (when (and stored (eq? value (creation-value stored)))
                      (allow! value)
                      (creation-into-set! stored (cons r (creation-into stored)))))
                  (if (and r (assq r objects) (pair? word) (eq? (car word) 'quad))
                      (let ((slot (cons (cadr word) value))
                            (c (hashtable-ref by-object r #f)))
                        (when c
                          (creation-slots-set! c (cons slot (creation-slots c))))
                        (walk (cadddr t) (cons (cons r (cons slot (cdr (assq r objects)))) objects)))
                      (walk (cadddr t) objects))))
               (else (walk (cadddr t) objects)))))
          ((letcont letcont-rec)
           (walk (caddr (car (cadr t))) objects)
           (walk (caddr t) objects))
          ((args) (walk (caddr t) objects))
          ((app)
           (let ((c (creation-of (caddr t))))
             (when (and c (eq? (caddr t) (creation-value c)))
               (allow! (caddr t))
               (creation-calls-set! c (cons (length (cddddr t)) (creation-calls c)))
               (creation-call-states-set! c (cons objects (creation-call-states c))))))
          (else (void))))

      (walk (fun-term fun) '())
      (for-each
       (lambda (c)
         (when (and (= (creation-fun c) g)
                    (not (= (use-count census (creation-value c))
                            (hashtable-ref allowed (creation-value c) 0))))
           (creation-escapes?-set! c #t)))
       (vector->list (hashtable-values by-object)))))


  ;; The words of def's captures
  (define (capture-words def)
    (map (lambda (i) (+ i 5)) (iota (def-captures def))))


  (define (captured-atom c word)
    (cdr (assv word (creation-slots c))))


  ;; Whether the captures of c are stored, as they end, in the objects' stores state
  (define (complete? c state)
    (let ((stores (assq (creation-object c) state)))
      (and stores
           (for-all (lambda (w) (equal? (assv w (cdr stores)) (assv w (creation-slots c))))
                    (capture-words (creation-def c))))))


  ;; Whether c, the one creation of its def, may be lifted with closures stored into: called
  ;; with as many atoms as parameters, each capture known and stored at each call, its value
  ;; only stored into closures
  (define (candidate? c i by-object funs)
    (let ((def (creation-def c)))
      (and i
           (vector-ref funs i)
           (not (= i (creation-fun c)))
           (positive? (def-captures def))
           (not (def-rest? def))
           (not (def-public? def))
           (not (def-noalloc? def))
           (eq? (def-result def) 'val)
           (not (creation-escapes? c))
           (for-all (lambda (n) (= n (length (def-params def)))) (creation-calls c))
           (for-all (lambda (w) (assv w (creation-slots c))) (capture-words def))
           (for-all (lambda (state) (complete? c state)) (creation-call-states c))
           (for-all (lambda (r) (hashtable-ref by-object r #f)) (creation-into c)))))


  ;; The candidates lifted: each stored only into others, its body's captures holding them
  ;; only called
  (define (lifted-members candidates index by-object funs)
    (let loop ((cs candidates))
      (let* ((kept (filter (lambda (c)
                             (and (for-all (lambda (r) (memq (hashtable-ref by-object r #f) cs))
                                           (creation-into c))
                                  (self-loads (vector-ref funs (hashtable-ref index (creation-def c) #f))
                                              c cs)))
                           cs)))
        (if (= (length kept) (length cs))
            cs
            (loop kept)))))


  ;; The member of cs whose value is the atom a, else #f
  (define (member-of a cs)
    (cond
     ((null? cs) #f)
     ((eq? a (creation-value (car cs))) (car cs))
     (else (member-of a (cdr cs)))))


  ;; The capture loads of the body of c's def, ((x . word) ...), when (self) is used only there
  ;; and each capture holding a member of cs is only called with as many atoms as its def's
  ;; parameters; else #f
  (define (self-loads fun c cs)
    (let ((def (creation-def c))
          (census (census (fun-term fun)))
          (loads '())
          (casts '())
          (callees (make-eq-hashtable))
          (ok? #t))
      (define (self-in? a)
        (cond
         ((equal? a '(self)) #t)
         ((pair? a) (exists self-in? (cdr a)))
         (else #f)))

      (let walk ((t (fun-term fun)))
        (case (car t)
          ((letprim)
           (let ((x (cadr t))
                 (op (caddr t)))
             (cond
              ((and (eq? (car op) 'load) x (equal? (cadr op) '(as-raw (self)))
                    (pair? (caddr op)) (eq? (car (caddr op)) 'quad)
                    (memv (cadr (caddr op)) (capture-words def)))
               (set! loads (cons (cons x (cadr (caddr op))) loads)))
              ((exists self-in? (op-atoms op)) (set! ok? #f))
              ((and (eq? (car op) 'as-val) x (symbol? (cadr op)))
               (set! casts (cons (cons x (cadr op)) casts))))
             (walk (cadddr t))))
          ((letcont letcont-rec)
           (walk (caddr (car (cadr t))))
           (walk (caddr t)))
          ((args) (walk (caddr t)))
          ((if) (when (exists self-in? (cdadr t)) (set! ok? #f)))
          ((throw) (when (exists self-in? (cddr t)) (set! ok? #f)))
          ((app)
           (when (exists self-in? (cdddr t))
             (set! ok? #f))
           (when (symbol? (caddr t))
             (hashtable-update! callees (caddr t)
                                (lambda (l) (cons (length (cddddr t)) l)) '())))
          ((selfcall) (set! ok? #f))
          (else (void))))
      (and ok?
           (for-all
            (lambda (l)
              (let ((m (member-of (captured-atom c (cdr l)) cs)))
                (or (not m)
                    ;; A closure of the group: only cast to a val, then only called
                    (let ((ys (filter (lambda (cast) (eq? (cdr cast) (car l))) casts))
                          (n (length (def-params (creation-def m)))))
                      (and (= (use-count census (car l)) (length ys))
                           (for-all (lambda (y)
                                      (let ((ns (hashtable-ref callees (car y) '())))
                                        (and (= (use-count census (car y)) (length ns))
                                             (for-all (lambda (k) (= k n)) ns))))
                                    ys))))))
            loads)
           loads)))


  ;; The members grouped by the closures they are stored into, in their order
  (define (groups members by-object)
    (let ((group (make-eq-hashtable)))
      (define (find! c)
        (let ((g (hashtable-ref group c c)))
          (if (eq? g c) c (find! g))))

      (for-each (lambda (c)
                  (for-each (lambda (r)
                              (let ((a (find! c))
                                    (b (find! (hashtable-ref by-object r #f))))
                                (unless (eq? a b)
                                  (hashtable-set! group a b))))
                            (creation-into c)))
                members)
      (let loop ((cs members) (roots '()))
        (cond
         ((pair? cs)
          (loop (cdr cs) (if (memq (find! (car cs)) roots) roots (cons (find! (car cs)) roots))))
         (else
          (map (lambda (root) (filter (lambda (c) (eq? (find! c) root)) members))
               (reverse roots)))))))


  ;; Gives each def of the group the group's captures but its members' as parameters, in
  ;; new-params: subst maps each capture load to its parameter, dropped the letprims that go,
  ;; callees each value calling a member to (def atom ...)
  (define (lift-group! group index funs by-object subst dropped callees new-params)
    (let ((captured
           (fold-left (lambda (acc c)
                        (fold-left (lambda (acc w)
                                     (let ((a (captured-atom c w)))
                                       (if (or (member-of a group) (member a acc))
                                           acc
                                           (append acc (list a)))))
                                   acc (capture-words (creation-def c))))
                      '() group))
          ;; Before any def's parameters change
          (loads (map (lambda (c)
                        (self-loads (vector-ref funs (hashtable-ref index (creation-def c) #f)) c group))
                      group)))
      (for-each
       (lambda (c loads)
         (let* ((def (creation-def c))
                (i (hashtable-ref index def #f))
                (fun (vector-ref funs i))
                (params (map (lambda (a) (gensym "c"))
                             captured)))
           (for-each
            (lambda (l)
              (let* ((a (captured-atom c (cdr l)))
                     (m (member-of a group)))
                (hashtable-set! dropped (car l) #t)
                (if m
                    ;; Its casts call m's def
                    (let walk ((t (fun-term fun)))
                      (case (car t)
                        ((letprim)
                         (let ((op (caddr t)))
                           (when (and (eq? (car op) 'as-val) (eq? (cadr op) (car l)))
                             (hashtable-set! dropped (cadr t) #t)
                             (hashtable-set! callees (cadr t) (cons (creation-def m) params))))
                         (walk (cadddr t)))
                        ((letcont letcont-rec)
                         (walk (caddr (car (cadr t))))
                         (walk (caddr t)))
                        ((args) (walk (caddr t)))
                        (else (void))))
                    (hashtable-set! subst (car l)
                                    `(as-raw ,(cdr (assoc a (map cons captured params))))))))
            loads)
           (hashtable-set! callees (creation-value c) (cons def captured))
           (def-params-set! def (append (def-params def) (map (lambda (p) (cons p 'val)) params)))
           (hashtable-set! new-params def params)))
       group loads)))


  ;; fun with the letprims of dropped gone, the atoms substituted, the calls of callees direct;
  ;; def's, when lifted, with its new parameters
  (define (lift-rewrite fun def subst dropped callees new-params)
    (define (atom a)
      (cond
       ((symbol? a) (hashtable-ref subst a a))
       ((pair? a) (cons (car a) (map atom (cdr a))))
       (else a)))

    (let ((params (hashtable-ref new-params def '())))
      `(fun ,(cadr fun) (,@(caddr fun) ,@params)
            ,(let walk ((t (fun-term fun)))
               (case (car t)
                 ((letprim)
                  (if (and (cadr t) (hashtable-ref dropped (cadr t) #f))
                      (walk (cadddr t))
                      `(letprim ,(cadr t) ,(map-op atom (caddr t)) ,(walk (cadddr t)))))
                 ((letcont letcont-rec)
                  (let ((b (car (cadr t))))
                    `(,(car t) ((,(car b) ,(cadr b) ,(walk (caddr b)))) ,(walk (caddr t)))))
                 ((args) `(args ,(cadr t) ,(walk (caddr t))))
                 ((if) `(if (,(caadr t) ,@(map atom (cdadr t))) ,@(cddr t)))
                 ((throw) `(throw ,(cadr t) ,@(map atom (cddr t))))
                 ((app)
                  (let ((callee (and (symbol? (caddr t)) (hashtable-ref callees (caddr t) #f))))
                    (if callee
                        `(app ,(cadr t) ,(car callee) ,(cadddr t) ,@(map atom (cddddr t))
                              ,@(map atom (cdr callee)))
                        `(app ,(cadr t) ,(atom (caddr t)) ,(cadddr t) ,@(map atom (cddddr t))))))
                 ((selfcall) `(selfcall ,@(map atom (cdr t))))
                 (else t)))))))
