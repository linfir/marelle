;; The CPS IR, between the core AST and the block IR (grammar in CPS.md), and its checker.
;;
;; fun   ::= (fun code (k) term)            k: its return continuation
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
;; op    ::= (low-op atom ...) | (as-val atom) | (stackcall kind code atom ...)
;;         | (literal x) | (move atom) | (cell-set! c atom)
;; atom  ::= x | a constant operand of the block IR: (quad n), "symbol", (arg n), (self),
;;           (as-val c), (as-raw atom)...

(library (cps)
  (export check-cps param-var param-kind op-atoms)
  (import (chezscheme)
          (utils))


  (define (param-var p)
    (if (pair? p) (cadr p) p))


  (define (param-kind p)
    (if (pair? p) (car p) 'val))


  ;; The atoms of op
  (define (op-atoms op)
    (case (car op)
      ((stackcall) (cdddr op))
      ((literal) '())
      (else (cdr op))))


  (define (check-cps fun)
    (let ((bound (make-eq-hashtable))
          (own (car (caddr fun))))
      (define (fail msg x)
        (error 'check-cps (format "~a in ~a" msg (cadr fun)) x))

      (define (bind! x)
        (unless (symbol? x)
          (fail "not a name" x))
        (when (hashtable-ref bound x #f)
          (fail "name bound twice" x))
        (hashtable-set! bound x #t))

      ;; vars: the variables in scope; conts: the continuations in scope, to their arity
      (define (atom! a vars)
        (cond
         ((symbol? a)
          (unless (memq a vars)
            (fail "variable out of scope" a)))
         ((pair? a)
          (for-each (lambda (x) (atom! x vars)) (cdr a)))))

      (define (cont-arity k conts)
        (cond
         ((eq? k own) 1)
         ((assq k conts) => cdr)
         (else (fail "continuation out of scope" k))))

      (define (check t vars conts)
        (case (car t)
          ((letprim)
           (for-each (lambda (a) (atom! a vars)) (op-atoms (caddr t)))
           (let ((x (cadr t)))
             (when x
               (bind! x))
             (check (cadddr t) (if x (cons x vars) vars) conts)))
          ((letcont letcont-rec)
           (let* ((b (car (cadr t)))
                  (k (car b))
                  (params (map param-var (cadr b)))
                  (inner (acons k (length params) conts)))
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
           (let ((n (cont-arity (cadr t) conts)))
             (unless (if (eq? (cadr t) own) (<= (length (cddr t)) 1) (= (length (cddr t)) n))
               (fail "wrong number of values thrown" t))))
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
      (check (cadddr fun) '() '()))))
