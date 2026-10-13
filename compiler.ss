#!/usr/bin/env -S scheme --script

(import (utils))
(load "ir.ss")
(load "frontend.ss")
(load "backend.ss")


(define *env* '())
(define *primitive-tops* '())
(define *primitive-exports* '())


(define (define-primitive symbol name arity . inliner)
  (let* ((proc (prim-proc-name name))
         (binding (make-primitive-binding proc arity name
                                          (and (pair? inliner) (car inliner)))))
    (set! *env* (acons symbol binding *env*))
    (set! *primitive-tops*
          (cons* (prim-function binding)
                 (proc-data proc arity (prim-fn-name name) (proc-name-label proc))
                 (proc-name-string proc symbol)
                 *primitive-tops*))
    (set! *primitive-exports* (cons (prim-proc-name name) *primitive-exports*))))


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
(set! *env* (bind-prelude-defs *env* *prelude-defs*))


(let* ((prelude? (member "--prelude" (command-line-arguments)))
       (tops (if prelude?
                 (append (reverse *primitive-tops*) (prelude->ir *env* *prelude-defs*))
                 (program->ir *env* (read-all))))
       (exports (if prelude?
                    (append (reverse *primitive-exports*) (prelude-exports *prelude-defs*))
                    program-exports)))
  (if (member "--ir" (command-line-arguments))
      (print-ir tops)
      (for-each print-asm (program->asm tops exports))))
