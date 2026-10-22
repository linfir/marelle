;; Contification, on the funs of a unit: a def always called from one other fun with the same
;; continuation k, and calling itself only in tail position, becomes a letcont-rec of that fun,
;; its calls throws to it, its return throws to k (after Fluet and Weeks, "Contification Using
;; Dominators", for the common case of a single caller). Its body moves into the caller, so the
;; def goes, and only one that nothing else may name: a lambda's (no env), not public, never a
;; value, (self) not in its body (a closure's once lifted).
;; The letcont-rec is placed in k's scope, around the main term of k's letcont, or around the
;; whole term when k is the caller's return; a plain letcont when the def never calls itself.

(library (contify)
  (export contify-unit)
  (import (chezscheme)
          (utils)
          (defs)
          (cps))


  ;; defs, funs and counters in the same order; (reoptimize def fun) optimizes a changed fun
  ;; again. Gives the funs, #f for a def that went.
  (define (contify-unit defs funs counters reoptimize)
    (let ((funs (list->vector funs))
          (counters (list->vector counters))
          (index (make-eq-hashtable)))
      (for-each (lambda (def i) (hashtable-set! index def i)) defs (iota (length defs)))
      ;; Rounds until none contifies anything: the candidates found at its start, but a def
      ;; that took in another this round (its calls changed), or one gone; each fun that took
      ;; some optimized again at the end
      (let ((changed (make-eqv-hashtable)))
        (let round ()
          (let ((callers (make-eqv-hashtable)))
            (for-each (lambda (c)
                        (let ((f (car c))
                              (g (cadr c)))
                          (unless (or (hashtable-ref callers f #f)
                                      (not (vector-ref funs f))
                                      (not (vector-ref funs g)))
                            (hashtable-set! callers g #t)
                            (hashtable-set! changed g #t)
                            (vector-set! funs g (contify (list-ref defs f) (vector-ref funs f)
                                                         (vector-ref funs g) (caddr c)
                                                         (vector-ref counters g)))
                            (vector-set! funs f #f))))
                      (find-candidates defs funs index))
            (unless (zero? (hashtable-size callers))
              (round))))
        (for-each (lambda (g)
                    (when (vector-ref funs g)
                      (vector-set! funs g (reoptimize (list-ref defs g) (vector-ref funs g)))))
                  (sort < (vector->list (hashtable-keys changed)))))
      (vector->list funs)))


  ;; The contifiable defs, as (f g k): f's index, its caller's and the continuation
  (define (find-candidates defs funs index)
    (let ((calls (make-eqv-hashtable))
          (escapes (make-eqv-hashtable))
          (procs (make-hashtable string-hash string=?)))
      ;; A value naming a def of the unit, or (self), makes it escape
      (define (atom! a g)
        (when (pair? a)
          (cond
           ((eq? (car a) 'self) (hashtable-set! escapes g #t))
           ((and (eq? (car a) 'as-val) (string? (cadr a)) (hashtable-ref procs (cadr a) #f))
            => (lambda (f) (hashtable-set! escapes f #t)))
           (else (for-each (lambda (a) (atom! a g)) (cdr a))))))

      (define (walk! t g)
        (case (car t)
          ((letprim)
           (for-each (lambda (a) (atom! a g)) (op-atoms (caddr t)))
           (walk! (cadddr t) g))
          ((letcont letcont-rec)
           (walk! (caddr (car (cadr t))) g)
           (walk! (caddr t) g))
          ((args) (walk! (caddr t) g))
          ((if) (for-each (lambda (a) (atom! a g)) (cdadr t)))
          ((throw) (for-each (lambda (a) (atom! a g)) (cddr t)))
          ((app)
           (let ((callee (caddr t)))
             (for-each (lambda (a) (atom! a g)) (cddddr t))
             (cond
              ((and (def? callee) (hashtable-ref index callee #f))
               => (lambda (f)
                    (hashtable-update! calls f (lambda (l) (cons (cons g (cadddr t)) l)) '())))
              (else (atom! callee g)))))
          (else (void))))

      (for-each (lambda (def i) (hashtable-set! procs (def-proc def) i)) defs (iota (length defs)))
      (for-each (lambda (fun g) (when fun (walk! (fun-term fun) g)))
                (vector->list funs) (iota (vector-length funs)))
      (filter (lambda (x) x)
              (map (lambda (def f)
                     (let ((fun (vector-ref funs f))
                           (cs (hashtable-ref calls f '())))
                       (and fun
                            (contifiable? def)
                            (not (hashtable-ref escapes f #f))
                            (let* ((own (car (caddr fun)))
                                   (inside (filter (lambda (c) (= (car c) f)) cs))
                                   (outside (filter (lambda (c) (not (= (car c) f))) cs)))
                              (and (pair? outside)
                                   (for-all (lambda (c) (eq? (cdr c) own)) inside)
                                   (for-all (lambda (c) (and (= (car c) (car (car outside)))
                                                             (eq? (cdr c) (cdr (car outside)))))
                                            outside)
                                   (vector-ref funs (car (car outside)))
                                   (not (eq? (cdr (car outside)) 'never))
                                   (not (rec-bound? (fun-term (vector-ref funs (car (car outside))))
                                                    (cdr (car outside))))
                                   (list f (car (car outside)) (cdr (car outside))))))))
                   defs (iota (length defs))))))


  (define (contifiable? def)
    (and (not (def-env def))
         (not (def-public? def))
         (not (def-noalloc? def))
         (not (def-rest? def))
         (eq? (def-result def) 'val)))


  ;; Whether k is a letcont-rec's continuation in t
  (define (rec-bound? t k)
    (let walk ((t t))
      (case (car t)
        ((letprim) (walk (cadddr t)))
        ((letcont letcont-rec)
         (let ((b (car (cadr t))))
           (or (and (eq? (car t) 'letcont-rec) (eq? (car b) k))
               (walk (caddr b))
               (walk (caddr t)))))
        ((args) (walk (caddr t)))
        (else #f))))


  ;; The fun of g, gfun, with the body of f, ffun, as a letcont-rec in the scope of k, and the
  ;; calls of f throws to it
  (define (contify f ffun gfun k counter)
    (let* ((fresh (lambda (x) (gensym (string-append (name-prefix x) (number->string (counter))))))
           (j (fresh (def-name f)))
           (subst (make-eq-hashtable))
           (params (map (lambda (p)
                          (let ((x (fresh (param-var p))))
                            (hashtable-set! subst (param-var p) x)
                            x))
                        (fun-params ffun)))
           (body (begin
                   (hashtable-set! subst (car (caddr ffun)) k)
                   (rename-term (fun-term ffun) subst fresh)))
           (loop (lambda (main) `(,(if (calls? body f) 'letcont-rec 'letcont) ((,j ,params ,body)) ,main)))
           (term (if (eq? k (car (caddr gfun)))
                     (loop (fun-term gfun))
                     (place (fun-term gfun) k loop))))
      `(,@(list-head gfun 3) ,(throws term f j))))


  ;; Whether t calls f
  (define (calls? t f)
    (let walk ((t t))
      (case (car t)
        ((letprim) (walk (cadddr t)))
        ((letcont letcont-rec) (or (walk (caddr (car (cadr t)))) (walk (caddr t))))
        ((args) (walk (caddr t)))
        ((app) (eq? (caddr t) f))
        (else #f))))


  ;; t with (wrap main) for the main term of k's letcont
  (define (place t k wrap)
    (let walk ((t t))
      (case (car t)
        ((letprim) `(letprim ,(cadr t) ,(caddr t) ,(walk (cadddr t))))
        ((letcont letcont-rec)
         (let ((b (car (cadr t))))
           (if (eq? (car b) k)
               `(,(car t) (,b) ,(wrap (caddr t)))
               `(,(car t) ((,(car b) ,(cadr b) ,(walk (caddr b)))) ,(walk (caddr t))))))
        ((args) `(args ,(cadr t) ,(walk (caddr t))))
        (else t))))


  ;; t with each call of f a throw to j, its mark gone
  (define (throws t f j)
    (let ((tags (make-eq-hashtable)))
      (let find ((t t))
        (case (car t)
          ((letprim) (find (cadddr t)))
          ((letcont letcont-rec) (find (caddr (car (cadr t)))) (find (caddr t)))
          ((args) (find (caddr t)))
          ((app) (when (and (eq? (caddr t) f) (cadr t)) (hashtable-set! tags (cadr t) #t)))
          (else (void))))
      (let walk ((t t))
        (case (car t)
          ((letprim) `(letprim ,(cadr t) ,(caddr t) ,(walk (cadddr t))))
          ((letcont letcont-rec)
           (let ((b (car (cadr t))))
             `(,(car t) ((,(car b) ,(cadr b) ,(walk (caddr b)))) ,(walk (caddr t)))))
          ((args)
           (if (hashtable-ref tags (cadr t) #f)
               (walk (caddr t))
               `(args ,(cadr t) ,(walk (caddr t)))))
          ((app) (if (eq? (caddr t) f) `(throw ,j ,@(cddddr t)) t))
          (else t))))))
