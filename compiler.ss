#!/usr/bin/env scheme-script

(import (chezscheme)
        (utils)
        (ir)
        (frontend)
        (backend))


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

(define-c-proc 'null? "is_null" 1)
(define-c-proc 'pair? "pair_p"  1)
(define-c-proc 'cons  "cons"    2)
(define-c-proc 'car   "car"     1)
(define-c-proc 'cdr   "cdr"     1)


(let* ((prelude? (member "--prelude" (command-line-arguments)))
       (tops (if prelude?
                 (reverse *prelude*)
                 (program->ir *env* (read-all))))
       (exports (if prelude?
                    (map cadr (filter (lambda (x) (eq? (car x) 'data)) tops))
                    '("scheme_main"))))
  (if (member "--ir" (command-line-arguments))
      (print-ir tops)
      (for-each print-asm (program->asm tops exports))))
