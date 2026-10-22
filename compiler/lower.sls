;; CPS to block IR, into the function being emitted (emit.sls).
;;
;; Each variable is an operand, a parameter its argument slot. A continuation is a label, emitted in place at its last use when
;; that use can fall into it (a throw, a call, a branch of an if: of two, the one bound last, the
;; then branch of the source), else after the term binding it. One used once gets its
;; parameters bound to the operands thrown to it; one used more, or a loop, gets a tmp per
;; parameter, set by each throw.

(library (lower)
  (export lower-function!)
  (import (chezscheme)
          (utils)
          (defs)
          (ops)
          (gc-check)
          (output)
          (emit)
          (compile)
          (cps))


  ;; params: the parameters, (var . kind); tmps: their tmps, or #f when bound at the use;
  ;; uses: its number of uses; left: those not lowered yet; order: the number of continuations
  ;; bound before it
  (define-record-type cont
    (fields label params body tmps rec? uses (mutable left) order (mutable placed?)))


  ;; Per lowering: the operands of the variables, the continuations, their use counts, the
  ;; return continuation and the def of the function
  (define vars (make-parameter #f))
  (define conts (make-parameter #f))
  (define uses (make-parameter #f))
  (define own (make-parameter #f))
  (define function-def (make-parameter #f))


  (define (lower-function! def fun)
    (parameterize ((vars (make-eq-hashtable))
                   (conts (make-eq-hashtable))
                   (uses (make-eq-hashtable))
                   (own (car (caddr fun)))
                   (function-def def))
      (for-each (lambda (p i)
                  (let ((arg `(arg ,(+ i 1))))
                    (bind! (param-var p) (if (eq? (param-kind p) 'raw) `(as-raw ,arg) arg))))
                (fun-params fun) (iota (length (fun-params fun))))
      (count-uses! (fun-term fun))
      (lower (fun-term fun))))


  (define (count-uses! t)
    (define (use! k)
      (hashtable-update! (uses) k (lambda (n) (+ n 1)) 0))

    (case (car t)
      ((letprim) (count-uses! (cadddr t)))
      ((letcont letcont-rec)
       (count-uses! (caddr (car (cadr t))))
       (count-uses! (caddr t)))
      ((if) (use! (caddr t)) (use! (cadddr t)))
      ((throw call) (use! (cadr t)))
      (else (void))))


  (define (atom->operand a)
    (cond
     ((symbol? a)
      (or (hashtable-ref (vars) a #f) (compile-error 'lower "unbound variable" a)))
     ((pair? a)
      (cons (car a) (map atom->operand (cdr a))))
     (else a)))


  (define (bind! x op)
    (hashtable-set! (vars) x op))


  (define (lower t)
    (case (car t)
      ((letprim)
       (lower-prim (cadr t) (caddr t))
       (lower (cadddr t)))
      ((letcont letcont-rec)
       (let* ((b (car (cadr t)))
              (k (car b))
              (rec? (eq? (car t) 'letcont-rec))
              (n (hashtable-ref (uses) k 0))
              (params (map (lambda (p) (cons (param-var p) (param-kind p))) (cadr b)))
              (c (make-cont (fresh-label) params (caddr b)
                            (and (or rec? (> n 1))
                                 (map-in-order (lambda (p) (fresh-tmp (cdr p))) params))
                            rec? n n (hashtable-size (conts)) #f)))
         (hashtable-set! (conts) k c)
         (when (cont-tmps c)
           (for-each (lambda (p tmp) (bind! (car p) tmp)) params (cont-tmps c)))
         (lower (caddr t))
         (unless (or (cont-placed? c) (zero? n))
           (start-block! (cont-label c))
           (lower (cont-body c)))))
      ((if)
       (lower-if (cadr t) (caddr t) (cadddr t)))
      ((throw)
       (lower-throw (cadr t) (map atom->operand (cddr t))))
      ((call)
       (lower-call (cadr t) (caddr t) (atom->operand (cadddr t))))
      ((selfcall)
       (emit! `(selfcall ,@(map atom->operand (cdr t)))))
      ((unreachable)
       (emit! '(unreachable)))
      (else
       (compile-error 'lower "not a term" t))))


  (define (cont-of k)
    (or (hashtable-ref (conts) k #f) (compile-error 'lower "unknown continuation" k)))


  (define (use! c)
    (cont-left-set! c (- (cont-left c) 1)))


  ;; Emitted here, at its last use
  (define (placeable? c)
    (and (zero? (cont-left c)) (not (cont-rec? c)) (not (cont-placed? c))))


  (define (place-cont! c)
    (cont-placed?-set! c #t)
    (when (> (cont-uses c) 1)
      (start-block! (cont-label c)))
    (lower (cont-body c)))


  ;; The continuation c entered with the operands ops, its use counted
  (define (enter! c ops)
    (if (cont-tmps c)
        (move! (cont-tmps c) ops)
        (for-each (lambda (p op) (bind! (car p) op)) (cont-params c) ops))
    (if (placeable? c)
        (place-cont! c)
        (emit! `(jmp ,(cont-label c)))))


  (define (lower-throw k ops)
    (if (eq? k (own))
        (let ((op (finish-result (function-def) (if (null? ops) '(void) (car ops)))))
          (unless (eq? (kind op) 'never)
            (emit! `(ret ,op))))
        (let ((c (cont-of k)))
          (use! c)
          (enter! c ops))))


  ;; Sets each tmp to its operand, all read before any is written: an operand reading one of
  ;; the tmps is copied first
  (define (move! tmps ops)
    (let ((ops (map-in-order (lambda (op)
                               (if (exists (lambda (tmp) (reads? op tmp)) tmps)
                                   (emit-op! 'mov (kind op) (list op))
                                   op))
                             ops)))
      (for-each (lambda (tmp op) (emit! `(mov ,tmp ,op))) tmps ops)))


  (define (reads? op tmp)
    (or (equal? op tmp)
        (and (pair? op) (exists (lambda (x) (reads? x tmp)) (cdr op)))))


  ;; The jumps of a test, (jump-if-true jump-if-false)
  (define (test-jumps name)
    (case name
      ((eq) '(je jne))
      ((true) '(jne je))
      (else (low-test name))))


  (define (lower-if test k1 k2)
    (let ((ops (map atom->operand (cdr test)))
          (jumps (test-jumps (car test)))
          (c1 (cont-of k1))
          (c2 (cont-of k2)))
      (if (eq? (car test) 'true)
          (emit! `(cmp ,(car ops) ,(immediate #f)))
          (emit! `(cmp ,@ops)))
      (use! c1)
      (use! c2)
      (cond
       ((eq? c1 c2)
        (if (placeable? c1)
            (place-cont! c1)
            (emit! `(jmp ,(cont-label c1)))))
       ((and (placeable? c1)
             (not (and (placeable? c2) (> (cont-order c2) (cont-order c1)))))
        (emit! `(,(cadr jumps) ,(cont-label c2)))
        (place-cont! c1))
       ((placeable? c2)
        (emit! `(,(car jumps) ,(cont-label c1)))
        (place-cont! c2))
       ;; As if c1 followed
       (else
        (emit! `(,(cadr jumps) ,(cont-label c2)))
        (emit! `(jmp ,(cont-label c1)))))))


  (define (lower-call k target f)
    (cond
     ((eq? k (own))
      (emit! `(tailcall ,target ,f)))
     ((eq? k 'never)
      (emit! `(call void ,target ,f))
      (emit! '(unreachable)))
     (else
      (let ((c (cont-of k)))
        (cond
         ((null? (cont-params c))
          (emit! `(call void ,target ,f))
          (lower-throw k '()))
         ((cont-tmps c)
          (emit! `(call ,(car (cont-tmps c)) ,target ,f))
          (use! c)
          (if (placeable? c)
              (place-cont! c)
              (emit! `(jmp ,(cont-label c)))))
         (else
          (let ((dst (fresh-tmp (cdar (cont-params c)))))
            (emit! `(call ,dst ,target ,f))
            (lower-throw k (list dst)))))))))


  (define (lower-prim x op)
    (case (car op)
      ((stackcall)
       (let ((ops (cons (def-code (caddr op)) (map atom->operand (cdddr op)))))
         (if x
             (bind! x (emit-op! 'stackcall (cadr op) ops))
             (emit! `(stackcall void ,@ops)))))
      ((move)
       (let ((a (atom->operand (cadr op))))
         (bind! x (emit-op! 'mov (kind a) (list a)))))
      ((cell-set!)
       (emit! `(mov ,(atom->operand (cadr op)) ,(atom->operand (caddr op)))))
      ;; as-val copies a raw slot into a tmp, since the GC scans only tmps
      ((as-val)
       (let ((a (atom->operand (cadr op))))
         (bind! x (if (zero? (raws-in a)) `(as-val ,a) (emit-op! 'mov 'val (list a))))))
      (else
       (when (and (eq? (car op) 'load-const) (equal? (cadr op) (literals-symbol)))
         (literal-loaded! (cadr (caddr op))))
       (let ((o (low-op (car op)))
             (ops (map atom->operand (cdr op))))
         (if (eq? (op-result o) 'void)
             (emit! `(,(op-instr o) ,@ops))
             (bind! x (emit-op! (op-instr o) (op-result o) ops))))))))
