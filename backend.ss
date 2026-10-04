;; Frames are heap objects, rbp pointing to the current one, n being the
;; function's arity:
;;
;;   rbp + 0            "frame_type"
;;   rbp + 8            length of the frame in words, header included (raw)
;;   rbp + 16           caller's frame, or 0 for scheme_main
;;   rbp + 24           return address (raw)
;;   rbp + 32           (self), the procedure object passed in r10, or 0 for scheme_main
;;   rbp + 40           (arg 1)
;;   ...
;;   rbp + 8*(n+4)      (arg n)
;;   rbp + 8*(n+5)      (tmp 1)
;;   ...
;;
;; C calls (ccall) follow the System V AMD64 ABI:
;;   - arguments in rdi, rsi, rdx, rcx, r8, r9, result in rax
;;   - rbx, rbp, rsp, r12-r15 are preserved, all other registers are clobbered
;;   - rsp is 16-byte aligned at the call; Scheme code never pushes, and
;;     scheme_enter aligns it once, so a plain call is always aligned
;;
;; Scheme calls (call, tailcall) use their own convention:
;;   - arguments in rdi, rsi, rdx, rcx, r8, r9 (at most 6), the procedure object
;;     in r10, the return address in r11, then a jmp to the code (no push)
;;   - the callee's prologue calls alloc_frame, which saves rbp, r10 and r11 in
;;     the new frame; the arguments are then stored in it
;;   - ret puts the result in rax, restores the caller's rbp and jumps to the
;;     saved return address
;;   - a tail call passes its own caller's frame and return address, so the
;;     callee returns directly there
;;   - only rbp and rsp are preserved: all values live in frames
;;   - the arity is checked by the caller, not the callee

(define arg-regs '(rdi rsi rdx rcx r8 r9))


(define (count-tmps x)
  (if (pair? x)
      (if (eq? (car x) 'tmp)
          (cadr x)
          (max (count-tmps (car x)) (count-tmps (cdr x))))
      0))


(define-record-type asm-context
  (fields fn arity ntmp (mutable code) ret-counter))


;; x is (function name (arity n) instr ...)
(define (new-asm-context x)
  (make-asm-context (cadr x) (cadr (caddr x)) (count-tmps (cdddr x)) '() (make-counter)))


(define (emit-asm! ctx . instrs)
  (asm-context-code-set! ctx (append (reverse instrs) (asm-context-code ctx))))


(define (slot ctx x)
  (case (car x)
    ((self)
     '(mem rbp 32))
    ((arg)
     `(mem rbp ,(* 8 (+ 4 (cadr x)))))
    ((tmp)
     `(mem rbp ,(* 8 (+ 4 (asm-context-arity ctx) (cadr x)))))
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
       (let ((label (ret-label! ctx)))
         (emit-asm! ctx
                    `(lea r11 (rip ,label))
                    '(jmp rax)
                    `(label ,label)))
       (store! ctx dst 'rax)))
    ((tailcall)
     (let ((target (cadr instr))
           (proc (caddr instr))
           (args (cdddr instr)))
       (load-args! ctx args)
       (load! ctx 'r10 proc)
       (load! ctx 'rax target)
       (emit-asm! ctx
                  '(mov r11 (mem rbp 24))
                  '(mov rbp (mem rbp 16))
                  '(jmp rax))))
    ((alloc)
     (let ((dst (cadr instr))
           (nwords (caddr instr))
           (tag (cadddr instr)))
       (emit-asm! ctx
                  `(mov rax ,nwords)
                  '(call "alloc_object"))
       (unless (= tag 0)
         (emit-asm! ctx `(or rax ,tag)))
       (store! ctx dst 'rax)))
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
                  '(mov r11 (mem rbp 24))
                  '(mov rbp (mem rbp 16))
                  '(jmp r11))))
    (else
     (error 'instr->asm! "unknown instruction" instr))))


(define (ret-label! ctx)
  (string-append ".L" (asm-context-fn ctx) "_ret"
                 (number->string ((asm-context-ret-counter ctx)))))


;; x is (function name (arity n) instr ...)
(define (function->asm x)
  (let* ((ctx (new-asm-context x))
         (fn (asm-context-fn ctx))
         (arity (asm-context-arity ctx))
         (nwords (+ 5 arity (asm-context-ntmp ctx))))
    (when (> arity (length arg-regs))
      (error 'function->asm "too many parameters" fn))
    (emit-asm! ctx
               '(text)
               `(label ,fn)
               `(mov rax ,nwords)
               '(call "alloc_frame"))
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
    (printf "qword ptr [rip + ~s]" (cadr x)))
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
