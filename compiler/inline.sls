;; Inlining, on the funs of a unit: a call (app tag def k atom ...) of a small def of the unit
;; becomes a renamed copy of the def's body, its parameters replaced by the atoms and its return
;; continuation by k. The defs are visited callees first, each optimized again once it changed,
;; so a caller inlines its callees' optimized bodies. The mark (args tag ...) of an inlined call
;; goes with it.
;; Not inlined: a def into itself, one with a rest parameter, a result other than val, captures,
;; or (self) in its body; anything into a noalloc def, whose calls stay errors.

(library (inline)
  (export inline-unit)
  (import (chezscheme)
          (utils)
          (defs)
          (cps))


  ;; The largest body inlined, in terms and letprims
  (define max-inline-size 16)


  ;; defs, funs and counters in the same order; (reoptimize def fun) optimizes a changed fun
  ;; again. Gives the funs.
  (define (inline-unit defs funs counters reoptimize)
    (let ((table (make-eq-hashtable))
          (state (make-eq-hashtable)))
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
                                      (and (not (eq? callee def))
                                           (hashtable-ref table callee #f)
                                           (eq? (hashtable-ref state callee #f) 'done)
                                           (inlinable? callee (fun-of callee))
                                           (fun-of callee)))))))
            (vector-set! entry 0 (if (eq? new fun) fun (reoptimize def new)))
            (hashtable-set! state def 'done))))

      (for-each (lambda (def fun counter)
                  (hashtable-set! table def (vector fun counter)))
                defs funs counters)
      (for-each visit! defs)
      (map fun-of defs)))


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


  (define (inlinable? def fun)
    (and (not (def-rest? def))
         (eq? (def-result def) 'val)
         (zero? (def-captures def))
         (<= (term-size (fun-term fun) (+ max-inline-size 1)) max-inline-size)
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


  ;; The name x without its number
  (define (name-prefix x)
    (let* ((s (symbol->string x))
           (n (let loop ((n (string-length s)))
                (if (and (> n 1) (char-numeric? (string-ref s (- n 1))))
                    (loop (- n 1))
                    n))))
      (substring s 0 n))))
