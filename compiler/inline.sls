;; Inlining, on the funs of a unit: a call (app tag def k atom ...) of a small def of the unit
;; becomes a renamed copy of the def's body, its parameters replaced by the atoms and its return
;; continuation by k. The defs are visited callees first, each optimized again once it changed,
;; so a caller inlines its callees' optimized bodies. The mark (args tag ...) of an inlined call
;; goes with it.
;; Not inlined: a def into itself, one with a rest parameter, a result other than val, captures,
;; or (self) in its body; anything into a noalloc def, whose calls stay errors.
;; The library's small procedures are inlined into programs too: the library exports their funs
;; in its interface (export-fun), a program imports them (import-fun) into extern-funs.

(library (inline)
  (export inline-unit extern-funs extern-defs export-fun import-fun)
  (import (chezscheme)
          (utils)
          (defs)
          (cps))


  ;; The largest body inlined, in terms and letprims
  (define max-inline-size 16)


  ;; The funs of the library's defs a program may inline, an eq hashtable, else #f
  (define extern-funs (make-parameter #f))


  ;; The defs outside the unit a call may name by their procedure object, a list
  (define extern-defs (make-parameter '()))


  ;; defs, funs and counters in the same order; (reoptimize def fun) optimizes a changed fun
  ;; again. Gives the funs.
  (define (inline-unit defs funs counters reoptimize)
    (let ((table (make-eq-hashtable))
          (state (make-eq-hashtable))
          (procs (known-procs (append defs (extern-defs)))))
      (define (fun-of def)
        (vector-ref (hashtable-ref table def #f) 0))

      ;; Visits def's callees first; a def being visited is called as it is
      (define (visit! def)
        (unless (hashtable-ref state def #f)
          (hashtable-set! state def 'visiting)
          (for-each visit! (callees (fun-term (fun-of def)) table))
          (let* ((entry (hashtable-ref table def #f))
                 (fun (vector-ref entry 0))
                 (new (if (def-noalloc? def)
                          fun
                          (inline-calls def fun (vector-ref entry 1)
                                    (lambda (callee)
                                      (cond
                                       ((eq? callee def) #f)
                                       ((hashtable-ref table callee #f)
                                        (and (eq? (hashtable-ref state callee #f) 'done)
                                             (inlinable? callee (fun-of callee) max-inline-size)
                                             (fun-of callee)))
                                       (else
                                        (and (extern-funs)
                                             (hashtable-ref (extern-funs) callee #f)))))))))
            (let ((new (direct-calls new procs)))
              (vector-set! entry 0 (if (eq? new fun) fun (reoptimize def new))))
            (hashtable-set! state def 'done))))

      (for-each (lambda (def fun counter)
                  (hashtable-set! table def (vector fun counter)))
                defs funs counters)
      (for-each visit! defs)
      (map fun-of defs)))


  ;; The defs a call may reach directly, by their procedure object's symbol
  (define (known-procs defs)
    (let ((procs (make-hashtable string-hash string=?)))
      (for-each (lambda (def)
                  (when (and (not (def-inline? def))
                             (not (def-noalloc? def))
                             (not (def-rest? def))
                             (eq? (def-result def) 'val)
                             (zero? (def-captures def)))
                    (hashtable-set! procs (def-proc def) def)))
                defs)
      procs))


  ;; fun with each call of a procedure object of procs, of its arity, a call of its def; fun
  ;; itself when there is none
  (define (direct-calls fun procs)
    (define (known t)
      (let ((callee (caddr t)))
        (and (pair? callee)
             (eq? (car callee) 'as-val)
             (string? (cadr callee))
             (let ((def (hashtable-ref procs (cadr callee) #f)))
               (and def (= (length (def-params def)) (length (cddddr t))) def)))))

    (let ((changed? #f))
      (let ((term (let walk ((t (fun-term fun)))
                    (case (car t)
                      ((letprim) `(letprim ,(cadr t) ,(caddr t) ,(walk (cadddr t))))
                      ((letcont letcont-rec)
                       (let ((b (car (cadr t))))
                         `(,(car t) ((,(car b) ,(cadr b) ,(walk (caddr b)))) ,(walk (caddr t)))))
                      ((args) `(args ,(cadr t) ,(walk (caddr t))))
                      ((app)
                       (let ((def (known t)))
                         (if def
                             (begin
                               (set! changed? #t)
                               `(app ,(cadr t) ,def ,@(cdddr t)))
                             t)))
                      (else t)))))
        (if changed? `(,@(list-head fun 3) ,term) fun))))


  ;; The defs of the unit fun's term calls
  (define (callees t table)
    (let walk ((t t) (acc '()))
      (case (car t)
        ((letprim) (walk (cadddr t) acc))
        ((letcont letcont-rec) (walk (caddr t) (walk (caddr (car (cadr t))) acc)))
        ((args) (walk (caddr t) acc))
        ((app)
         (let ((callee (caddr t)))
           (if (and (def? callee) (hashtable-ref table callee #f) (not (memq callee acc)))
               (cons callee acc)
               acc)))
        (else acc))))


  (define (inlinable? def fun limit)
    (and (not (def-rest? def))
         (eq? (def-result def) 'val)
         (zero? (def-captures def))
         (<= (term-size (fun-term fun) (+ limit 1)) limit)
         (not (mentions-self? (fun-term fun)))))


  ;; The size of t, counting up to limit
  (define (term-size t limit)
    (let walk ((t t) (n 0))
      (if (>= n limit)
          n
          (case (car t)
            ((letprim args) (walk (cadddr* t) (+ n 1)))
            ((letcont letcont-rec)
             (walk (caddr t) (walk (caddr (car (cadr t))) (+ n 1))))
            (else (+ n 1))))))


  ;; The term inside a letprim or an args
  (define (cadddr* t)
    (if (eq? (car t) 'args) (caddr t) (cadddr t)))


  (define (mentions-self? t)
    (define (atom? a)
      (and (pair? a) (or (eq? (car a) 'self) (exists atom? (cdr a)))))

    (let walk ((t t))
      (case (car t)
        ((letprim) (or (exists atom? (op-atoms (caddr t))) (walk (cadddr t))))
        ((letcont letcont-rec) (or (walk (caddr (car (cadr t)))) (walk (caddr t))))
        ((args) (walk (caddr t)))
        ((if) (exists atom? (cdadr t)))
        ((throw) (exists atom? (cddr t)))
        ((app) (or (atom? (caddr t)) (exists atom? (cddddr t))))
        (else #f))))


  ;; fun with each call of a def for which (body-of def) gives a fun replaced by its body;
  ;; fun itself when there is none
  (define (inline-calls def fun counter body-of)
    (let ((inlined (make-eq-hashtable))
          (changed? #f))
      (define (fresh x)
        (gensym (string-append (name-prefix x) (number->string (counter)))))

      ;; The inlined calls' tags, first, so that their marks go
      (define (find! t)
        (case (car t)
          ((letprim) (find! (cadddr t)))
          ((letcont letcont-rec) (find! (caddr (car (cadr t)))) (find! (caddr t)))
          ((args) (find! (caddr t)))
          ((app)
           (let ((callee (caddr t)))
             (when (and (def? callee)
                        (= (length (cddddr t)) (length (def-params callee)))
                        (body-of callee))
               (set! changed? #t)
               (when (cadr t)
                 (hashtable-set! inlined (cadr t) #t)))))
          (else (void))))

      (define (walk t)
        (case (car t)
          ((letprim) `(letprim ,(cadr t) ,(caddr t) ,(walk (cadddr t))))
          ((letcont letcont-rec)
           (let ((b (car (cadr t))))
             `(,(car t) ((,(car b) ,(cadr b) ,(walk (caddr b)))) ,(walk (caddr t)))))
          ((args)
           (if (hashtable-ref inlined (cadr t) #f)
               (walk (caddr t))
               `(args ,(cadr t) ,(walk (caddr t)))))
          ((app)
           (let* ((callee (caddr t))
                  (callee-fun (and (def? callee)
                                   (= (length (cddddr t)) (length (def-params callee)))
                                   (body-of callee))))
             (if callee-fun
                 (let ((subst (make-eq-hashtable)))
                   (hashtable-set! subst (car (caddr callee-fun)) (cadddr t))
                   (for-each (lambda (p a) (hashtable-set! subst (param-var p) a))
                             (fun-params callee-fun) (cddddr t))
                   (rename-term (fun-term callee-fun) subst fresh))
                 t)))
          (else t)))

      (find! (fun-term fun))
      (if changed?
          `(,@(list-head fun 3) ,(walk (fun-term fun)))
          fun)))


  ;; The datum of the fun of def for the interface, its defs as (def name), when it is
  ;; inlinable and names only defs for which (item? def), else #f. A literal is a load-const of
  ;; the library's cell, so a program's copy gets the library's object, eq? to it.
  (define (export-fun def fun item?)
    (let ((ok? (inlinable? def fun max-inline-size)))
      (let ((x (map-defs fun (lambda (d)
                               (unless (item? d)
                                 (set! ok? #f))
                               `(def ,(def-name d))))))
        (and ok? x))))


  ;; The fun of an interface datum, each (def name) the def (lookup name), each symbol string
  ;; one for which (symbol? s); #f when either fails
  (define (import-fun x lookup symbol?)
    (define (atom? a)
      (cond
       ((string? a) (symbol? a))
       ((pair? a) (for-all atom? (cdr a)))
       (else #t)))

    (let ((ok? (let walk ((t (fun-term x)))
                 (case (car t)
                   ((letprim) (and (for-all atom? (op-atoms (caddr t))) (walk (cadddr t))))
                   ((letcont letcont-rec) (and (walk (caddr (car (cadr t)))) (walk (caddr t))))
                   ((args) (walk (caddr t)))
                   ((if) (for-all atom? (cdadr t)))
                   ((throw) (for-all atom? (cddr t)))
                   ((app) (and (atom? (caddr t)) (for-all atom? (cddddr t))))
                   (else #t)))))
      (let ((fun (map-defs x (lambda (d)
                               (or (lookup (cadr d))
                                   (begin (set! ok? #f) d))))))
        (and ok? fun))))


  ;; fun with each def d, a callee or a stackcall's, replaced by (f d)
  (define (map-defs fun f)
    (define (callee? x)
      (or (def? x) (and (pair? x) (eq? (car x) 'def))))

    `(,@(list-head fun 3)
      ,(let walk ((t (fun-term fun)))
         (case (car t)
           ((letprim)
            (let ((op (caddr t)))
              `(letprim ,(cadr t)
                        ,(if (eq? (car op) 'stackcall)
                             `(stackcall ,(cadr op) ,(f (caddr op)) ,@(cdddr op))
                             op)
                        ,(walk (cadddr t)))))
           ((letcont letcont-rec)
            (let ((b (car (cadr t))))
              `(,(car t) ((,(car b) ,(cadr b) ,(walk (caddr b)))) ,(walk (caddr t)))))
           ((args) `(args ,(cadr t) ,(walk (caddr t))))
           ((app)
            (if (callee? (caddr t))
                `(app ,(cadr t) ,(f (caddr t)) ,@(cdddr t))
                t))
           (else t))))))
