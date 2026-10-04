#!/usr/bin/env -S scheme --script

(load "utils.ss")
(load "ir.ss")
(load "frontend.ss")
(load "backend.ss")


(define *env* '())
(define *prelude* '())


(define (define-primitive symbol name arity . inliner)
  (let ((binding (make-primitive-binding (prim-proc-name name) arity name
                                         (and (pair? inliner) (car inliner)))))
    (set! *env* (acons symbol binding *env*))
    (set! *prelude*
          (cons* (prim-function binding)
                 (proc-data (prim-proc-name name) arity (prim-fn-name name))
                 *prelude*))))


(define-primitive 'eq? "eq" 2 inline-eq)

(define-primitive '+   "int_add" 2)
(define-primitive '-   "int_sub" 2)
(define-primitive '*   "int_mul" 2)
(define-primitive '=   "int_eq"  2)
(define-primitive '<   "int_lt"  2)
(define-primitive '<=  "int_leq" 2)
(define-primitive '>   "int_gt"  2)
(define-primitive '>=  "int_geq" 2)

(define-primitive 'null? "is_null" 1 inline-null)
(define-primitive 'pair? "pair_p"  1)
(define-primitive 'cons  "cons"    2 inline-cons)
(define-primitive 'car   "car"     1)
(define-primitive 'cdr   "cdr"     1)


(define *prelude-defs* (scan-prelude (with-input-from-file "prelude.ss" read-all)))
(set! *env* (bind-defs *env* *prelude-defs*))


(let* ((prelude? (member "--prelude" (command-line-arguments)))
       (tops (if prelude?
                 (append (reverse *prelude*) (prelude->ir *env* *prelude-defs*))
                 (program->ir *env* (read-all))))
       (exports (if prelude?
                    (append (map (lambda (x) (prim-proc-name (primitive-binding-name (cdr x))))
                                 (filter (lambda (x) (primitive-binding? (cdr x))) *env*))
                            (map def-proc *prelude-defs*))
                    '("scheme_main" "scheme_globals" "scheme_nglobals"))))
  (if (member "--ir" (command-line-arguments))
      (print-ir tops)
      (for-each print-asm (program->asm tops exports))))
