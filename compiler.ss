#!/usr/bin/env scheme-script

(import (chezscheme)
        (utils)
        (ir)
        (frontend)
        (backend))


(define *env* '())
(define *prelude* '())


(define (define-c-proc name c-name arity)
  (let* ((proc (string-append c-name "_proc"))
         (fn (string-append c-name "_code"))
         (args (map (lambda (i) (list 'arg (+ i 1))) (iota arity))))
    (set! *env* (acons name proc *env*))
    (set! *prelude*
          (cons* (list 'function fn (list 'arity arity)
                       (cons* 'ccall '(tmp 1) c-name args)
                       '(ret (tmp 1)))
                 (proc-data proc arity fn)
                 *prelude*))))


(define-c-proc 'eq? "eq" 2)

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
