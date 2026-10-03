;; Stack frame of a function, ntmp being the highest tmp number:
;;
;;   rbp + 8            return address
;;   rbp                saved rbp
;;   rbp - 8            (self), the procedure object passed in r10
;;   rbp - 16           (tmp 1)
;;   ...
;;   rbp - 8*(ntmp+1)   (tmp ntmp)
;;   rbp - 8*(ntmp+2)   (arg 1)
;;   rbp - 8*(ntmp+3)   (arg 2)
;;   ...
;;   (padding to keep rsp 16-byte aligned)

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


(define (emit-asm! ctx . instrs)
  (asm-context-code-set! ctx (append (reverse instrs) (asm-context-code ctx))))


(define (slot ctx x)
  (case (car x)
    ((self)
     (list 'mem 'rbp -8))
    ((tmp)
     (list 'mem 'rbp (* -8 (+ 1 (cadr x)))))
    ((arg)
     (list 'mem 'rbp (* -8 (+ 1 (asm-context-ntmp ctx) (cadr x)))))
    (else
     (error 'slot "not a slot operand" x))))


(define (load! ctx reg x)
  (cond
   ((string? x)
    (emit-asm! ctx `(lea ,reg (rip ,x))))
   ((pair? x)
    (case (car x)
      ((self tmp arg)
       (emit-asm! ctx `(mov ,reg ,(slot ctx x))))
      ((imm)
       (emit-asm! ctx `(mov ,reg ,(encode-immediate (cadr x)))))
      ((quad)
       (emit-asm! ctx `(mov ,reg ,(cadr x))))
      (else
       (error 'load! "unknown operand" x))))
   (else
    (error 'load! "unknown operand" x))))


(define (store! ctx x reg)
  (emit-asm! ctx `(mov ,(slot ctx x) ,reg)))


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
       (emit-asm! ctx `(call ,target))
       (unless (eq? dst 'void)
         (store! ctx dst 'rax))))
    ((call)
     (let ((dst (cadr instr))
           (target (caddr instr))
           (proc (cadddr instr))
           (args (cddddr instr)))
       (load-args! ctx args)
       (load! ctx 'r10 proc)
       (load! ctx 'rax target)
       (emit-asm! ctx '(call rax))
       (store! ctx dst 'rax)))
    ((tailcall)
     (let ((target (cadr instr))
           (proc (caddr instr))
           (args (cdddr instr)))
       (load-args! ctx args)
       (load! ctx 'r10 proc)
       (load! ctx 'rax target)
       (emit-asm! ctx
		  '(mov rsp rbp)
		  '(pop rbp)
		  '(jmp rax))))
    ((store)
     (let ((base (cadr instr))
           (index (caddr instr))
           (src (cadddr instr)))
       (load! ctx 'rax base)
       (load! ctx 'rcx src)
       (emit-asm! ctx
		  '(and rax -8)
		  `(mov (mem rax ,(* 8 index)) rcx))))
    ((load)
     (let ((dst (cadr instr))
           (base (caddr instr))
           (index (cadddr instr)))
       (load! ctx 'rax base)
       (emit-asm! ctx
		  '(and rax -8)
		  `(mov rax (mem rax ,(* 8 index))))
       (store! ctx dst 'rax)))
    ((cmp)
     (let ((a (cadr instr))
           (b (caddr instr)))
       (load! ctx 'rax a)
       (load! ctx 'rcx b)
       (emit-asm! ctx '(cmp rax rcx))))
    ((je jne jmp)
     (let ((op (car instr))
           (label (cadr instr)))
       (emit-asm! ctx `(,op ,(label-name ctx label)))))
    ((def)
     (let ((label (cadr instr)))
       (emit-asm! ctx `(label ,(label-name ctx label)))))
    ((ret)
     (let ((src (cadr instr)))
       (load! ctx 'rax src)
       (emit-asm! ctx
		  '(mov rsp rbp)
		  '(pop rbp)
		  '(ret))))
    (else
     (error 'instr->asm! "unknown instruction" instr))))


;; x is (function name (arity n) instr ...)
(define (function->asm x)
  (let* ((ctx (new-asm-context x))
         (fn (asm-context-fn ctx))
         (arity (cadr (caddr x)))
         (nslots (+ 1 (asm-context-ntmp ctx) arity))
         (size (* 8 (round-up-even nslots))))
    (when (> arity (length arg-regs))
      (error 'function->asm "too many parameters" fn))
    (emit-asm! ctx
               '(text)
               `(label ,fn)
               '(push rbp)
               '(mov rbp rsp)
               `(sub rsp ,size))
    (store! ctx '(self) 'r10)
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


(define (program->asm tops exports)
  (append '((intel_syntax noprefix))
          (map (lambda (name) `(globl ,name)) exports)
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
        (newline))))
