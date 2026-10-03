#!/usr/bin/env -S scheme --script

(load "utils.ss")
(load "ir.ss")
(load "frontend.ss")
(load "backend.ss")


(define *env* '())
(define *prelude* '())


(define (define-c-proc name c-name arity . inliner)
  (set! *env* (acons name (make-primitive-binding (prim-proc-name c-name) arity c-name
                                                  (and (pair? inliner) (car inliner)))
                     *env*))
  (set! *prelude*
        (cons* (prim-function c-name arity)
               (proc-data (prim-proc-name c-name) arity (prim-fn-name c-name))
               *prelude*)))


(define-c-proc 'eq? "eq" 2 inline-eq)

(define-c-proc '+   "int_add" 2)
(define-c-proc '-   "int_sub" 2)
(define-c-proc '*   "int_mul" 2)
(define-c-proc '=   "int_eq"  2)
(define-c-proc '<   "int_lt"  2)
(define-c-proc '<=  "int_leq" 2)
(define-c-proc '>   "int_gt"  2)
(define-c-proc '>=  "int_geq" 2)

(define-c-proc 'null? "is_null" 1 inline-null)
(define-c-proc 'pair? "pair_p"  1)
(define-c-proc 'cons  "cons"    2)
(define-c-proc 'car   "car"     1)
(define-c-proc 'cdr   "cdr"     1)


(define *prelude-defs* (scan-prelude (with-input-from-file "prelude.ss" read-all)))
(set! *env* (bind-defs *env* *prelude-defs*))


(let* ((prelude? (member "--prelude" (command-line-arguments)))
       (tops (if prelude?
                 (append (reverse *prelude*) (prelude->ir *env* *prelude-defs*))
                 (program->ir *env* (read-all))))
       (exports (if prelude?
                    (append (map (lambda (x) (prim-proc-name (primitive-binding-c-name (cdr x))))
                                 (filter (lambda (x) (primitive-binding? (cdr x))) *env*))
                            (map def-proc *prelude-defs*))
                    '("scheme_main"))))
  (if (member "--ir" (command-line-arguments))
      (print-ir tops)
      (for-each print-asm (program->asm tops exports))))
