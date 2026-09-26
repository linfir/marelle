#!/usr/bin/env scheme-script

(import (chezscheme)
        (utils)
        (frontend)
        (backend))

;; ----------------------------------------------------------------------------

;; (define (emit-proc proc arity fn)
;;   (printf "data ~a = align 8 { l $procedure_type, l ~a, l ~a }\n" proc arity fn))

;; (define (emit expr)
;;   (define (p . x)
;;     (for-each display x)
;;     (newline))

;;   (define (pi . x)
;;     (display "    ")
;;     (apply p x))

;;   (unless (null? expr)
;;     (case (car expr)
;;       ((main)
;;        (let ((body (cdr expr)))
;;          (display "export function l $entry() {\n")
;;          (for-each emit body)
;;          (display "}\n\n")))

;;       ((function)
;;        (let ((name (cadr expr))
;;              (vars (caddr expr))
;;              (body (cdddr expr)))
;;          (emit-proc name (length vars) (string-append name "_fn"))
;;          (printf "function l ~a_fn(~{l ~a~^, ~}) {\n" name vars)
;;          (for-each emit body)
;;          (display "}\n\n")))

;;       ((copy)
;;        (pi (cadr expr) " =l copy " (caddr expr)))

;;       ((call)
;;        (printf "    ~a =l call ~a(~{l ~a~^, ~})\n" (cadr expr) (caddr expr) (cdddr expr)))

;;       ((ret)
;;        (pi "ret " (cadr expr)))

;;       ((label)
;;        (p (cadr expr)))

;;       ((jmp)
;;        (pi "jmp " (cadr expr)))

;;       ((cnel)
;;        (printf "    ~a =l cnel ~a, ~a\n" (list-ref expr 1) (list-ref expr 2) (list-ref expr 3)))

;;       ((jnz)
;;        (printf "    jnz ~a, ~a, ~a\n" (list-ref expr 1) (list-ref expr 2) (list-ref expr 3)))

;;       ((phi)
;;        (printf "    ~a =l phi ~a ~a, ~a ~a\n"
;;                (list-ref expr 1) (list-ref expr 2) (list-ref expr 3)
;;                (list-ref expr 4) (list-ref expr 5)))

;;       (else
;;        (error 'emit "unknown instruction " expr)))))

;; ----------------------------------------------------------------------------

(define *env* '())
(define *prelude* '())


(define (define-c-proc name c-name arity)
  (let* ((n (proc-counter))
         (proc (proc-name n))
         (fn (fn-name n))
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


(define (print-instr instr)
  (if (eq? (car instr) 'def)
      (printf "  ~s\n" instr)
      (printf "    ~s\n" instr)))


(define (print-top x)
  (case (car x)
    ((data)
     (printf "~s\n\n" x))
    ((function)
     (printf "(function ~s ~s\n" (cadr x) (caddr x))
     (for-each print-instr (cdddr x))
     (display ")\n\n"))))


(let ((tops (append (reverse *prelude*)
                    (cpl-program *env* (read-all)))))
  (if (member "--ir" (command-line-arguments))
      (for-each print-top tops)
      (for-each print-asm (program->asm tops))))
