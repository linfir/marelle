;; Stack frame of a function, ntmp being the highest tmp number:
;;
;;   rbp + 8            return address
;;   rbp                saved rbp
;;   rbp - 8            (tmp 1)
;;   ...
;;   rbp - 8*ntmp       (tmp ntmp)
;;   rbp - 8*(ntmp+1)   (arg 1)
;;   rbp - 8*(ntmp+2)   (arg 2)
;;   ...
;;   (padding to keep rsp 16-byte aligned, if ntmp + arity is odd)

(library (backend)
  (export program->asm print-asm)
  (import (chezscheme)
          (only (utils) round-up-even)
          (only (ir) encode-immediate))


  (define arg-regs '(rdi rsi rdx rcx r8 r9))


  (define (count-tmps x)
    (if (pair? x)
        (if (eq? (car x) 'tmp)
            (cadr x)
            (max (count-tmps (car x)) (count-tmps (cdr x))))
        0))


  (define-record-type asm-context
    (fields fn ntmp (mutable code)))


  ;; x is (function name (arity n) instr ...)
  (define (new-asm-context x)
    (make-asm-context (cadr x) (count-tmps (cdddr x)) '()))


  (define (emit! ctx . instrs)
    (asm-context-code-set! ctx (append (reverse instrs) (asm-context-code ctx))))


  (define (slot ctx x)
    (case (car x)
      ((tmp)
       (list 'mem 'rbp (* -8 (cadr x))))
      ((arg)
       (list 'mem 'rbp (* -8 (+ (asm-context-ntmp ctx) (cadr x)))))
      (else
       (error 'slot "not a slot operand" x))))


  (define (load! ctx reg x)
    (cond
     ((string? x)
      (emit! ctx `(lea ,reg (rip ,x))))
     ((pair? x)
      (case (car x)
        ((tmp arg)
         (emit! ctx `(mov ,reg ,(slot ctx x))))
        ((imm)
         (emit! ctx `(mov ,reg ,(encode-immediate (cadr x)))))
        ((quad)
         (emit! ctx `(mov ,reg ,(cadr x))))
        (else
         (error 'load! "unknown operand" x))))
     (else
      (error 'load! "unknown operand" x))))


  (define (store! ctx x reg)
    (emit! ctx `(mov ,(slot ctx x) ,reg)))


  (define (load-args! ctx args)
    (when (> (length args) (length arg-regs))
      (error 'load-args! "too many arguments" args))
    (let loop ((args args)
               (regs arg-regs))
      (unless (null? args)
        (load! ctx (car regs) (car args))
        (loop (cdr args) (cdr regs)))))


  (define (label-name ctx label)
    (string-append ".L" (asm-context-fn ctx) "_" (number->string (cadr label))))


  (define (instr->asm! ctx instr)
    (case (car instr)
      ((mov)
       (let ((dst (cadr instr))
             (src (caddr instr)))
         (load! ctx 'rax src)
         (store! ctx dst 'rax)))
      ((ccall)
       (let ((dst (cadr instr))
             (target (caddr instr))
             (args (cdddr instr)))
         (load-args! ctx args)
         (emit! ctx `(call ,target))
         (store! ctx dst 'rax)))
      ((call)
       (let ((dst (cadr instr))
             (target (caddr instr))
             (args (cdddr instr)))
         (load-args! ctx args)
         (load! ctx 'rax target)
         (emit! ctx '(call rax))
         (store! ctx dst 'rax)))
      ((tailcall)
       (let ((target (cadr instr))
             (args (cddr instr)))
         (load-args! ctx args)
         (load! ctx 'rax target)
         (emit! ctx '(mov rsp rbp) '(pop rbp) '(jmp rax))))
      ((cmp)
       (let ((a (cadr instr))
             (b (caddr instr)))
         (load! ctx 'rax a)
         (load! ctx 'rcx b)
         (emit! ctx '(cmp rax rcx))))
      ((je jmp)
       (let ((op (car instr))
             (label (cadr instr)))
         (emit! ctx `(,op ,(label-name ctx label)))))
      ((def)
       (let ((label (cadr instr)))
         (emit! ctx `(label ,(label-name ctx label)))))
      ((ret)
       (let ((src (cadr instr)))
         (load! ctx 'rax src)
         (emit! ctx '(mov rsp rbp) '(pop rbp) '(ret))))
      (else
       (error 'instr->asm! "unknown instruction" instr))))


  ;; x is (function name (arity n) instr ...)
  (define (function->asm x)
    (let* ((ctx (new-asm-context x))
           (fn (asm-context-fn ctx))
           (arity (cadr (caddr x)))
           (nslots (+ (asm-context-ntmp ctx) arity))
           (size (* 8 (round-up-even nslots))))
      (when (> arity (length arg-regs))
        (error 'function->asm "too many parameters" fn))
      (emit! ctx '(text) `(label ,fn) '(push rbp) '(mov rbp rsp))
      (when (> size 0)
        (emit! ctx `(sub rsp ,size)))
      (let loop ((i 1)
                 (regs arg-regs))
        (when (<= i arity)
          (store! ctx (list 'arg i) (car regs))
          (loop (+ i 1) (cdr regs))))
      (for-each (lambda (instr) (instr->asm! ctx instr)) (cdddr x))
      (reverse (asm-context-code ctx))))


  (define (top->asm x)
    (case (car x)
      ((data)
       `((section ".data.rel.ro")
         (balign 8)
         (label ,(cadr x))
         (quad ,@(map cadr (cddr x)))))
      ((function)
       (function->asm x))
      (else (error 'top->asm "unknown top-level entry" x))))


  (define (program->asm tops)
    (append '((intel_syntax noprefix)
              (globl "scheme_main"))
            (apply append (map top->asm tops))
            '((section ".note.GNU-stack" "" %progbits))))


  (define *directives* '(intel_syntax globl text section balign quad))


  (define (print-operand x)
    (cond
     ((or (symbol? x) (number? x))
      (display x))
     ((string? x)
      (write x))
     ((eq? (car x) 'mem)
      (let ((off (caddr x)))
        (printf "qword ptr [~a ~a ~a]" (cadr x) (if (< off 0) "-" "+") (abs off))))
     ((eq? (car x) 'rip)
      (printf "[rip + ~s]" (cadr x)))
     (else (error 'print-operand "unknown operand" x))))


  (define (print-asm x)
    (if (eq? (car x) 'label)
        (printf "~s:\n" (cadr x))
        (begin
          (printf "    ~a~a" (if (memq (car x) *directives*) "." "") (car x))
          (let loop ((ops (cdr x))
                     (sep " "))
            (unless (null? ops)
              (display sep)
              (print-operand (car ops))
              (loop (cdr ops) ", ")))
          (newline)))))
