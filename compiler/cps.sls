;; The CPS IR, between the core AST and the block IR (grammar in CPS.md), its checker, and what the
;; passes on it share: kinds, census, effects, renaming.
;;
;; fun   ::= (fun code (k param ...) term)  k: its return continuation; the parameters are
;;                                          bound to the frame's argument slots
;; term  ::= (letprim x op term) | (letprim #f op term)
;;         | (letcont ((k (param ...) term)) term)
;;         | (letcont-rec ((k (param ...) term)) term)      a loop
;;         | (if (test atom ...) k1 k2)       test: a low test, eq (two vals), true (not #f)
;;         | (throw k atom ...)               to a continuation, a return when k is the fun's
;;         | (call k target f)                a call of the filled frame f: target a code symbol,
;;                                            or frame (its self's code); k a continuation, the
;;                                            fun's (a tail call), or never
;;         | (selfcall proc atom ...)          a self tail call reusing the frame
;;         | (unreachable)
;;         | (app tag callee k atom ...)       a call of callee, a def or an atom, with the
;;                                            arguments atom ...; k as for call
;;         | (args tag term)                  the arguments of the app tag computed in term
;; app and args, the converter's, become frames, stores and calls (expand-frames) before the
;; lowering.
;; param ::= x | (raw x)
;; op    ::= (low-op atom ...) | (as-val atom) | (stackcall kind def atom ...)
;;         | (literal x) | (move atom) | (cell-set! c atom)
;; atom  ::= x | a constant operand of the block IR: (quad n), "symbol", (self), (as-val c),
;;           (as-raw atom)...

(library (cps)
  (export check-cps param-var param-kind op-atoms fun-params fun-term
          cps-kinds op-kind census use-count bound-op cps-effect rename-term)
  (import (chezscheme)
          (utils)
          (defs)
          (ops)
          (compile))


  (define (param-var p)
    (if (pair? p) (cadr p) p))


  (define (param-kind p)
    (if (pair? p) (car p) 'val))


  (define (fun-params fun)
    (cdr (caddr fun)))


  (define (fun-term fun)
    (cadddr fun))


  ;; The atoms of op
  (define (op-atoms op)
    (case (car op)
      ((stackcall) (cdddr op))
      ((literal) '())
      (else (cdr op))))


  ;; op with each atom a replaced by (f a)
  (define (map-op f op)
    (case (car op)
      ((stackcall) `(,@(list-head op 3) ,@(map f (cdddr op))))
      ((literal) op)
      (else (cons (car op) (map f (cdr op))))))


  ;; The kind of op's result, given the kinds of its atoms
  (define (op-kind op atom-kind)
    (case (car op)
      ((as-val literal) 'val)
      ((stackcall) (cadr op))
      ((move) (atom-kind (cadr op)))
      ((cell-set!) 'void)
      (else (op-result (low-op (car op))))))


  ;; The kinds of fun's variables, from their bindings: an eq hashtable
  (define (cps-kinds fun)
    (let ((kinds (make-eq-hashtable)))
      (define (atom-kind a)
        (if (symbol? a) (hashtable-ref kinds a #f) (kind a)))

      (define (param! p)
        (hashtable-set! kinds (param-var p) (param-kind p)))

      (define (walk t)
        (case (car t)
          ((letprim)
           (when (cadr t)
             (hashtable-set! kinds (cadr t) (op-kind (caddr t) atom-kind)))
           (walk (cadddr t)))
          ((letcont letcont-rec)
           (let ((b (car (cadr t))))
             (for-each param! (cadr b))
             (walk (caddr b))
             (walk (caddr t))))
          ((args) (walk (caddr t)))
          (else (void))))

      (for-each param! (fun-params fun))
      (walk (fun-term fun))
      kinds))


  ;; The census of a term: the uses of each variable and continuation, and each letprim's op
  (define-record-type census-record
    (fields uses ops))


  (define (census t)
    (let ((uses (make-eq-hashtable))
          (ops (make-eq-hashtable)))
      (define (use! x)
        (hashtable-update! uses x (lambda (n) (+ n 1)) 0))

      (define (atom! a)
        (cond
         ((symbol? a) (use! a))
         ((pair? a) (for-each atom! (cdr a)))))

      (define (walk t)
        (case (car t)
          ((letprim)
           (for-each atom! (op-atoms (caddr t)))
           (when (cadr t)
             (hashtable-set! ops (cadr t) (caddr t)))
           (walk (cadddr t)))
          ((letcont letcont-rec)
           (walk (caddr (car (cadr t))))
           (walk (caddr t)))
          ((if)
           (for-each atom! (cdadr t))
           (use! (caddr t))
           (use! (cadddr t)))
          ((throw)
           (use! (cadr t))
           (for-each atom! (cddr t)))
          ((call)
           (use! (cadr t))
           (atom! (cadddr t)))
          ((app)
           (atom! (caddr t))
           (use! (cadddr t))
           (for-each atom! (cddddr t)))
          ((selfcall) (for-each atom! (cdr t)))
          ((args) (walk (caddr t)))
          (else (void))))

      (walk t)
      (make-census-record uses ops)))


  (define (use-count c x)
    (hashtable-ref (census-record-uses c) x 0))


  ;; The op binding the variable x, or #f for a parameter
  (define (bound-op c x)
    (hashtable-ref (census-record-ops c) x #f))


  ;; The effect of op, as in ops.sls: pure (deleted when unused, duplicated, moved), reads,
  ;; writes, allocates (a safepoint) or never. A stackcall writes unless its def is pure; a
  ;; literal is pure after the init calls; a move reads its cell, a cell-set! writes it. An app
  ;; or a call does everything, a safepoint too.
  ;; The GC rule: no raw variable live across a safepoint, so a pass that moves or shares a raw
  ;; letprim must not extend its life across one (check-gc-safety, after the lowering, makes a
  ;; slip a compile error).
  (define (cps-effect op)
    (case (car op)
      ((as-val literal) 'pure)
      ((stackcall) (if (def-pure? (caddr op)) 'pure 'writes))
      ((move) 'reads)
      ((cell-set!) 'writes)
      (else (op-effect (low-op (car op))))))


  ;; A copy of the term t with a fresh name, (fresh x), for each of its binders (variables,
  ;; continuations, args tags); a free name x is replaced by its entry in subst, an eq hashtable
  ;; (to an atom, or a continuation for a continuation), if any. For inlining and duplicating.
  (define (rename-term t subst fresh)
    (define (name x)
      (hashtable-ref subst x x))

    (define (atom a)
      (cond
       ((symbol? a) (name a))
       ((pair? a) (cons (car a) (map atom (cdr a))))
       (else a)))

    (define (bind! x)
      (let ((y (fresh x)))
        (hashtable-set! subst x y)
        y))

    (define (param! p)
      (if (pair? p) (list (car p) (bind! (cadr p))) (bind! p)))

    (define (walk t)
      (case (car t)
        ((letprim)
         (let* ((op (map-op atom (caddr t)))
                (x (and (cadr t) (bind! (cadr t)))))
           `(letprim ,x ,op ,(walk (cadddr t)))))
        ((letcont letcont-rec)
         (let* ((b (car (cadr t)))
                (rec? (eq? (car t) 'letcont-rec))
                (k (if rec? (bind! (car b)) (fresh (car b))))
                (params (map param! (cadr b)))
                (body (walk (caddr b))))
           (unless rec?
             (hashtable-set! subst (car b) k))
           `(,(car t) ((,k ,params ,body)) ,(walk (caddr t)))))
        ((if) `(if (,(caadr t) ,@(map atom (cdadr t))) ,(name (caddr t)) ,(name (cadddr t))))
        ((throw) `(throw ,(name (cadr t)) ,@(map atom (cddr t))))
        ((call) `(call ,(name (cadr t)) ,(caddr t) ,(atom (cadddr t))))
        ((app)
         `(app ,(and (cadr t) (name (cadr t))) ,(atom (caddr t)) ,(name (cadddr t))
               ,@(map atom (cddddr t))))
        ((selfcall) `(selfcall ,@(map atom (cdr t))))
        ((args)
         (let ((tag (bind! (cadr t))))
           `(args ,tag ,(walk (caddr t)))))
        (else t)))

    (walk t))


  (define (check-cps fun)
    (let ((bound (make-eq-hashtable))
          (own (car (caddr fun)))
          (kinds (cps-kinds fun)))
      (define (fail msg x)
        (compile-error 'check-cps (format "~a in ~a" msg (cadr fun)) x))

      (define (bind! x)
        (unless (symbol? x)
          (fail "not a name" x))
        (when (hashtable-ref bound x #f)
          (fail "name bound twice" x))
        (hashtable-set! bound x #t))

      ;; vars: the variables in scope; conts: the continuations in scope, to their parameters'
      ;; kinds
      (define (atom! a vars)
        (cond
         ((symbol? a)
          (unless (memq a vars)
            (fail "variable out of scope" a)))
         ((pair? a)
          (for-each (lambda (x) (atom! x vars)) (cdr a)))))

      (define (atom-kind a)
        (if (symbol? a) (hashtable-ref kinds a #f) (kind a)))

      (define (cont-kinds k conts)
        (cond
         ((assq k conts) => cdr)
         (else (fail "continuation out of scope" k))))

      (define (cont-arity k conts)
        (if (eq? k own) 1 (length (cont-kinds k conts))))

      (define (check t vars conts)
        (case (car t)
          ((letprim)
           (for-each (lambda (a) (atom! a vars)) (op-atoms (caddr t)))
           (let ((x (cadr t)))
             (when x
               (bind! x)
               (unless (memq (hashtable-ref kinds x #f) '(val raw))
                 (fail "variable bound to an op of kind void or never" t)))
             (check (cadddr t) (if x (cons x vars) vars) conts)))
          ((letcont letcont-rec)
           (let* ((b (car (cadr t)))
                  (k (car b))
                  (params (map param-var (cadr b)))
                  (inner (acons k (map param-kind (cadr b)) conts)))
             (bind! k)
             (for-each bind! params)
             (check (caddr b) (append params vars) (if (eq? (car t) 'letcont-rec) inner conts))
             (check (caddr t) vars inner)))
          ((if)
           (for-each (lambda (a) (atom! a vars)) (cdadr t))
           (for-each (lambda (k)
                       (unless (and (not (eq? k own)) (= (cont-arity k conts) 0))
                         (fail "branch to a continuation with parameters" t)))
                     (cddr t)))
          ((throw)
           (for-each (lambda (a) (atom! a vars)) (cddr t))
           (let ((k (cadr t)))
             (unless (if (eq? k own)
                         (<= (length (cddr t)) 1)
                         (equal? (map atom-kind (cddr t)) (cont-kinds k conts)))
               (fail "wrong number or kinds of values thrown" t))))
          ((call)
           (atom! (cadddr t) vars)
           (unless (or (eq? (cadr t) 'never) (<= (cont-arity (cadr t) conts) 1))
             (fail "call returning to a continuation of several parameters" t)))
          ((selfcall)
           (for-each (lambda (a) (atom! a vars)) (cdr t)))
          ((args)
           (check (caddr t) vars conts))
          ((app)
           (for-each (lambda (a) (atom! a vars)) (cons (caddr t) (cddddr t)))
           (unless (or (eq? (cadddr t) 'never) (<= (cont-arity (cadddr t) conts) 1))
             (fail "call returning to a continuation of several parameters" t)))
          ((unreachable) (void))
          (else
           (fail "not a term" t))))

      (unless (and (pair? fun) (eq? (car fun) 'fun))
        (fail "not a fun" fun))
      (bind! own)
      (for-each bind! (map param-var (fun-params fun)))
      (check (fun-term fun) (map param-var (fun-params fun)) '()))))
