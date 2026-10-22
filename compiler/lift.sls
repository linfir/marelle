;; Lifting known closures, on the funs of a unit: a closure created once, in a fun g, and only
;; ever called there (never a value), becomes a def of no captures. Its captures become extra
;; parameters, each call (app tag v k atom ...) through its value v a direct call of its def with
;; the captured atoms after the arguments, its body's capture loads those parameters; a capture
;; holding the closure itself is no parameter, the body's calls through it direct calls passing
;; its parameters on. The closure's allocation is left unused, for the simplifier.
;; A closure is (alloc), (copy-words r "x_proc" (quad 5)), then a store of each capture i at
;; word i + 5, of r or of its value (as-val r); its body loads capture i from (self).

(library (lift)
  (export lift-unit)
  (import (chezscheme)
          (utils)
          (defs)
          (cps))


  ;; A closure's creation: its def, its value, the fun creating it, the captured atoms by word at
  ;; each of its calls, and whether something else uses its value
  (define-record-type creation
    (fields def value fun (mutable slots) (mutable calls) (mutable escapes?)))


  ;; defs, funs and counters in the same order; (reoptimize def fun) optimizes a changed fun
  ;; again. Gives the funs.
  (define (lift-unit defs funs counters reoptimize)
    (let ((procs (make-hashtable string-hash string=?))
          (index (make-eq-hashtable))
          (creations (make-eq-hashtable))
          (by-value (make-eq-hashtable)))
      (for-each (lambda (def i)
                  (hashtable-set! procs (def-proc def) def)
                  (hashtable-set! index def i))
                defs (iota (length defs)))
      (for-each (lambda (fun g) (when fun (find-creations! fun g procs creations by-value)))
                funs (iota (length funs)))
      (let* ((funs (list->vector funs))
             (lifted (filter (lambda (def)
                               (let ((cs (hashtable-ref creations def '())))
                                 (and (= (length cs) 1)
                                      (liftable? def (hashtable-ref index def #f) (car cs) funs))))
                             defs)))
        (if (null? lifted)
            (vector->list funs)
            (let ((subst (make-eq-hashtable))
                  (dropped (make-eq-hashtable))
                  (callees (make-eq-hashtable))
                  (new-params (make-eq-hashtable))
                  (changed (make-eqv-hashtable)))
              (for-each (lambda (def)
                          (let* ((i (hashtable-ref index def #f))
                                 (c (car (hashtable-ref creations def '())))
                                 (counter (list-ref counters i))
                                 (fun (vector-ref funs i)))
                            (lift! def fun c counter subst dropped callees new-params)
                            (hashtable-set! changed i #t)
                            (hashtable-set! changed (creation-fun c) #t)))
                        lifted)
              (for-each (lambda (i)
                          (let ((def (list-ref defs i)))
                            (vector-set! funs i
                                         (reoptimize def (lift-rewrite (vector-ref funs i) def subst dropped
                                                                  callees new-params)))))
                        (sort < (vector->list (hashtable-keys changed))))
              (vector->list funs))))))


  ;; The object an atom names: the variable r of an alloc, through (as-val r) and casts
  (define (object-of a aliases)
    (cond
     ((symbol? a) (hashtable-ref aliases a #f))
     ((and (pair? a) (memq (car a) '(as-raw as-val)) (pair? (cdr a))) (object-of (cadr a) aliases))
     (else #f)))


  ;; Records the closures fun g creates, in creations (def to creations), by-value (value to
  ;; creation)
  (define (find-creations! fun g procs creations by-value)
    (let ((aliases (make-eq-hashtable))
          (procs-of (make-eq-hashtable))
          (census (census (fun-term fun))))
      (define (creation-of a)
        (let ((r (object-of a aliases)))
          (and r (hashtable-ref by-value r #f))))

      ;; objects: per allocated r, the stores seen on this path, ((r . ((word . atom) ...)) ...)
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
                  (when (and r (string? (caddr op)) (assq r objects))
                    (hashtable-set! procs-of r (caddr op))))
                (walk (cadddr t) objects))
               ((as-val)
                (let ((r (object-of (cadr op) aliases)))
                  (when (and x r)
                    (hashtable-set! aliases x r)
                    (let ((def (hashtable-ref procs (hashtable-ref procs-of r "") #f)))
                      (when (and def (not (hashtable-ref by-value r #f)))
                        (let ((c (make-creation def x g #f '() #f)))
                          (hashtable-set! by-value r c)
                          (hashtable-update! creations def (lambda (l) (cons c l)) '()))))))
                (walk (cadddr t) objects))
               ((store)
                (let ((r (object-of (cadr op) aliases))
                      (word (caddr op)))
                  (walk (cadddr t)
                        (if (and r (assq r objects) (pair? word) (eq? (car word) 'quad))
                            (cons (cons r (cons (cons (cadr word) (cadddr op))
                                                (cdr (assq r objects))))
                                  objects)
                            objects))))
               (else (walk (cadddr t) objects)))))
          ((letcont letcont-rec)
           (walk (caddr (car (cadr t))) objects)
           (walk (caddr t) objects))
          ((args) (walk (caddr t) objects))
          ((app)
           (let ((c (creation-of (caddr t))))
             (when (and c (symbol? (caddr t)))
               (let ((r (object-of (caddr t) aliases)))
                 (creation-calls-set! c (cons (length (cddddr t)) (creation-calls c)))
                 (let ((slots (cdr (or (assq r objects) (list r)))))
                   (if (creation-slots c)
                       (unless (equal? slots (creation-slots c))
                         (creation-escapes?-set! c #t))
                       (creation-slots-set! c slots)))))))
          (else (void))))

      (walk (fun-term fun) '())
      ;; A value used but as a callee, a store's base, or stored into its own closure, escapes
      (let ((allowed (make-eq-hashtable)))
        (let count ((t (fun-term fun)))
          (case (car t)
            ((letprim)
             (let ((op (caddr t)))
               (when (eq? (car op) 'store)
                 (let ((c (creation-of (cadr op))))
                   (when (and c (equal? (cadr op) `(as-raw ,(creation-value c))))
                     (hashtable-update! allowed (creation-value c) (lambda (n) (+ n 1)) 0)
                     (when (eq? (cadddr op) (creation-value c))
                       (hashtable-update! allowed (creation-value c) (lambda (n) (+ n 1)) 0))))))
             (count (cadddr t)))
            ((letcont letcont-rec)
             (count (caddr (car (cadr t))))
             (count (caddr t)))
            ((args) (count (caddr t)))
            ((app)
             (let ((c (creation-of (caddr t))))
               (when (and c (eq? (caddr t) (creation-value c)))
                 (hashtable-update! allowed (caddr t) (lambda (n) (+ n 1)) 0))))
            (else (void))))
        (vector-for-each
         (lambda (c)
           (when (and (= (creation-fun c) g)
                      (not (= (use-count census (creation-value c))
                              (hashtable-ref allowed (creation-value c) 0))))
             (creation-escapes?-set! c #t)))
         (hashtable-values by-value)))))


  ;; The words of def's captures
  (define (capture-words def)
    (map (lambda (i) (+ i 5)) (iota (def-captures def))))


  ;; Whether the closure of def, created once by c, may be lifted: only called, with as many
  ;; atoms as parameters, each capture known at the calls; its body using (self) only to load
  ;; captures, the ones holding the closure only to call it
  (define (liftable? def i c funs)
    (and i
         (vector-ref funs i)
         (vector-ref funs (creation-fun c))
         (not (= i (creation-fun c)))
         (positive? (def-captures def))
         (not (def-rest? def))
         (not (def-public? def))
         (not (def-noalloc? def))
         (eq? (def-result def) 'val)
         (not (creation-escapes? c))
         (pair? (creation-calls c))
         (for-all (lambda (n) (= n (length (def-params def)))) (creation-calls c))
         (creation-slots c)
         (for-all (lambda (w) (assv w (creation-slots c))) (capture-words def))
         (self-loads (vector-ref funs i) def c)))


  ;; The capture loads of fun's body, ((x . word) ...), when (self) is used only there and each
  ;; capture holding the closure is only called; else #f
  (define (self-loads fun def c)
    (let ((census (census (fun-term fun)))
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
              (or (not (eq? (cdr (assv (cdr l) (creation-slots c))) (creation-value c)))
                  ;; The closure itself: only cast to a val, then only called
                  (let ((ys (filter (lambda (cast) (eq? (cdr cast) (car l))) casts)))
                    (and (= (use-count census (car l)) (length ys))
                         (for-all (lambda (y)
                                    (let ((ns (hashtable-ref callees (car y) '())))
                                      (and (= (use-count census (car y)) (length ns))
                                           (for-all (lambda (n) (= n (length (def-params def)))) ns))))
                                  ys)))))
            loads)
           loads)))


  ;; Gives def its captures as parameters, in new-params: subst maps each capture load to its
  ;; parameter, dropped the letprims that go, callees each value calling the closure to
  ;; (def atom ...)
  (define (lift! def fun c counter subst dropped callees new-params)
    (let* ((loads (self-loads fun def c))
           (words (filter (lambda (w) (not (eq? (cdr (assv w (creation-slots c))) (creation-value c))))
                          (capture-words def)))
           (params (map (lambda (w) (gensym (string-append "c" (number->string (counter))))) words)))
      (for-each (lambda (l)
                  (hashtable-set! dropped (car l) #t)
                  (cond
                   ((memv (cdr l) words)
                    (hashtable-set! subst (car l) `(as-raw ,(cdr (assv (cdr l) (map cons words params))))))
                   (else
                    ;; The closure itself: its casts call def
                    (let walk ((t (fun-term fun)))
                      (case (car t)
                        ((letprim)
                         (let ((op (caddr t)))
                           (when (and (eq? (car op) 'as-val) (eq? (cadr op) (car l)))
                             (hashtable-set! dropped (cadr t) #t)
                             (hashtable-set! callees (cadr t) (cons def params))))
                         (walk (cadddr t)))
                        ((letcont letcont-rec)
                         (walk (caddr (car (cadr t))))
                         (walk (caddr t)))
                        ((args) (walk (caddr t)))
                        (else (void)))))))
                loads)
      (hashtable-set! callees (creation-value c)
                      (cons def (map (lambda (w) (cdr (assv w (creation-slots c)))) words)))
      (def-params-set! def (append (def-params def) (map (lambda (p) (cons p 'val)) params)))
      (hashtable-set! new-params def params)))


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
