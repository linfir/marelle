;; Frames are heap objects, rbp pointing to the current one, n being the
;; function's arity and m its number of tmps:
;;
;;   rbp + 0            frame type (43 << 3)
;;   rbp + 8            length of the frame in words, header included (raw)
;;   rbp + 16           number of raw slots at the end, not scanned (raw)
;;   rbp + 24           return address (raw)
;;   rbp + 32           caller's frame, or 0 for scheme_main
;;   rbp + 40           (self), the procedure object, or 0 for scheme_main
;;   rbp + 48           (arg 1)
;;   ...
;;   rbp + 8*(n+5)      (arg n)
;;   rbp + 8*(n+6)      (tmp 1)
;;   ...
;;   rbp + 8*(n+m+5)    (tmp m)
;;   rbp + 8*(n+m+6)    (raw 1)
;;   ...
;;
;; Noalloc functions, which never allocate or call Scheme, have their frame
;; on the machine stack, rbp pointing to it, n being the arity and m the
;; number of tmps and raws:
;;
;;   rbp + 8*(n+1)      (arg n)
;;   ...
;;   rbp + 16           (arg 1)
;;   rbp + 8            return address
;;   rbp + 0            caller's rbp
;;   rbp - 8            (tmp 1)
;;   ...                then the other tmps and the raws
;;   rbp - 8*m          rsp
;;
;; Noalloc calls (stackcall):
;;   - the caller pushes the arguments, the last first, any number, then calls;
;;     the callee pushes rbp, points rbp to it and makes room for its tmps and
;;     raws; it returns with leave and ret, the result in rax, and the caller
;;     pops the arguments
;;   - only rbp and rsp are preserved, as in Scheme calls: the caller saves the
;;     registers it needs around the call
;;
;; Scheme calls (call, tailcall) use their own convention, without argument registers:
;;   - the length of a function's frame is the word before its code
;;   - the caller allocates the callee's frame (allocframe, the alloc_frame stub,
;;     which sets the header and self) and stores the arguments in it, any number;
;;     call then stores the return address and rbp in it, points rbp to it and
;;     jumps to the code; the callee's prologue only stores the number of raw slots
;;   - ret puts the result in rax, restores the caller's rbp and jumps to the
;;     saved return address
;;   - a tail call gives the new frame its own return address and caller's frame,
;;     so the callee returns directly there
;;   - a tail call to the function itself (selfcall) reuses the frame: the
;;     arguments and self are moved into it, then a jump to the body
;;   - only rbp and rsp are preserved: all values live in frames
;;   - the arity is checked by the caller, not the callee

(library (asm)
  (export compile-asm)
  (import (chezscheme)
          (utils)
          (reg-alloc)
          (peephole))


  (define syscall-regs '(rdi rsi rdx r10 r8 r9))
  ;; Never holding operands, which are in rbx, r12-r15 or slots (see reg-alloc.sls)
  (define selfcall-regs '(rax rcx rdx rsi rdi r8 r9 r10 r11))


  (define frame-type (* 43 8))
  (define frame-nraw 16)
  (define frame-ret 24)
  (define frame-caller 32)
  (define frame-self 40)


  (define (count-slots tag x)
    (if (pair? x)
        (if (eq? (car x) tag)
            (cadr x)
            (max (count-slots tag (car x)) (count-slots tag (cdr x))))
        0))


  (define-record-type asm-context
    (fields fn noalloc? arity ntmp nraw (mutable code) ret-counter))


  ;; x is the function, blocks its blocks after register allocation
  (define (new-asm-context x blocks)
    (make-asm-context (cadr x) (eq? (car x) 'noalloc) (cadr (caddr x))
                      (count-slots 'tmp blocks) (count-slots 'raw blocks)
                      '() (make-counter)))


  (define (emit-asm! ctx . instrs)
    (asm-context-code-set! ctx (append (reverse instrs) (asm-context-code ctx))))


  ;; Word i of the frame after the header, from 1: the args, the tmps, then the raws
  (define (frame-word ctx i)
    (let ((arity (asm-context-arity ctx)))
      (cond
       ((not (asm-context-noalloc? ctx)) `(mem rbp ,(+ frame-self (* 8 i))))
       ((<= i arity) `(mem rbp ,(+ 8 (* 8 i))))
       (else `(mem rbp ,(* -8 (- i arity)))))))


  (define (slot ctx x)
    (case (car x)
      ((self)
       `(mem rbp ,frame-self))
      ((arg)
       (frame-word ctx (cadr x)))
      ((tmp)
       (frame-word ctx (+ (asm-context-arity ctx) (cadr x))))
      ((raw)
       (frame-word ctx (+ (asm-context-arity ctx) (asm-context-ntmp ctx) (cadr x))))
      (else
       (compile-error 'slot "not a slot operand" x))))


  (define (load! ctx reg x)
    (cond
     ((string? x)
      (emit-asm! ctx `(lea ,reg (rip ,x))))
     ((pair? x)
      (case (car x)
        ((reg)
         (unless (eq? reg (cadr x))
           (emit-asm! ctx `(mov ,reg ,(cadr x)))))
        ((self tmp raw arg)
         (emit-asm! ctx `(mov ,reg ,(slot ctx x))))
        ((quad)
         (emit-asm! ctx `(mov ,reg ,(cadr x))))
        (else
         (compile-error 'load! "unknown operand" x))))
     (else
      (compile-error 'load! "unknown operand" x))))


  (define (store! ctx x reg)
    (if (eq? (car x) 'reg)
        (unless (eq? (cadr x) reg)
          (emit-asm! ctx `(mov ,(cadr x) ,reg)))
        (emit-asm! ctx `(mov ,(slot ctx x) ,reg))))


  (define (imm32? n)
    (<= (- (expt 2 31)) n (- (expt 2 31) 1)))


  ;; x as an x86 operand, without loading it: a register, a slot or an imm32, else #f
  (define (operand ctx x)
    (and (pair? x)
         (case (car x)
           ((reg) (cadr x))
           ((self tmp raw arg) (slot ctx x))
           ((quad) (and (imm32? (cadr x)) (cadr x)))
           (else #f))))


  ;; x as an operand, loaded into reg unless it has one
  (define (source! ctx x reg)
    (or (operand ctx x)
        (begin
          (load! ctx reg x)
          reg)))


  ;; x as a register or memory operand, loaded into reg if it is neither
  (define (place! ctx x reg)
    (let ((op (operand ctx x)))
      (if (and op (not (number? op)))
          op
          (begin
            (load! ctx reg x)
            reg))))


  (define (result-register dst)
    (if (register? dst) (cadr dst) 'rax))


  (define (in-register! ctx x reg)
    (if (register? x)
        (cadr x)
        (begin
          (load! ctx reg x)
          reg)))


  ;; push takes a register, a slot or an imm32
  (define (push-args! ctx args)
    (for-each (lambda (x) (emit-asm! ctx `(push ,(source! ctx x 'rax)))) (reverse args)))


  (define (load-args! ctx args regs)
    (when (> (length args) (length regs))
      (compile-error 'load-args! "too many arguments" args))
    (for-each (lambda (reg x) (load! ctx reg x)) (list-head regs (length args)) args))


  (define (body-label ctx)
    (string-append ".L" (asm-context-fn ctx) "_body"))


  (define (label-name ctx label)
    (string-append ".L" (asm-context-fn ctx) "_" (number->string label)))


  (define (ret-label! ctx)
    (string-append ".L" (asm-context-fn ctx) "_ret"
                   (number->string ((asm-context-ret-counter ctx)))))


  ;; The memory operand for word index of the address base, using rax and rcx
  (define (element! ctx base index)
    (let ((base (in-register! ctx base 'rax)))
      (if (eq? (car index) 'quad)
          `(mem ,base ,(* 8 (cadr index)))
          `(mem ,base ,(in-register! ctx index 'rcx) 8))))


  ;; The memory operand of size mem8 or mem32 for byte index of the address base, using rax
  ;; and rcx
  (define (byte-element! ctx base index size)
    (let ((base (in-register! ctx base 'rax)))
      (if (eq? (car index) 'quad)
          `(,size ,base ,(cadr index))
          `(,size ,base ,(in-register! ctx index 'rcx)))))


  ;; The low 32 bits of a register; writing them zeroes the high ones
  (define (register32 reg)
    (cdr (assq reg '((rax . eax) (rbx . ebx) (rcx . ecx) (rdx . edx) (rsi . esi) (rdi . edi)
                     (r8 . r8d) (r9 . r9d) (r10 . r10d) (r11 . r11d) (r12 . r12d) (r13 . r13d)
                     (r14 . r14d) (r15 . r15d)))))


  ;; Register d = a op b, b not being in d
  (define (arith! ctx op d a b)
    (let ((b-op (operand ctx b)))
      (if (and (eq? op 'imul) (number? b-op))
          (emit-asm! ctx `(imul ,d ,(place! ctx a d) ,b-op))
          (begin
            (load! ctx d a)
            (emit-asm! ctx `(,op ,d ,(or b-op (begin (load! ctx 'rcx b) 'rcx))))))))


  ;; A constant count is an immediate, any other goes in cl
  (define (shift! ctx op dst a b)
    (let ((d (result-register dst)))
      (if (and (eq? (car b) 'quad) (<= 0 (cadr b) 63))
          (begin
            (load! ctx d a)
            (emit-asm! ctx `(,op ,d ,(cadr b))))
          (begin
            (load! ctx 'rcx b)
            (load! ctx d a)
            (emit-asm! ctx `(,op ,d cl))))
      (store! ctx dst d)))


  ;; The frame in rax, and the code in rcx unless target is a symbol, before rbp moves
  (define (enter-frame! ctx target frame)
    (load! ctx 'rax frame)
    (unless (string? target)
      (emit-asm! ctx
                 `(mov rcx (mem rax ,frame-self))
                 '(mov rcx (mem rcx 16)))))


  (define (leave! ctx)
    (emit-asm! ctx
               `(mov r11 (mem rbp ,frame-ret))
               `(mov rbp (mem rbp ,frame-caller))))


  (define (instr->asm! ctx instr)
    (case (car instr)
      ;; Directly into or from a register or from an imm32, else through rax
      ((mov)
       (let ((dst (cadr instr))
             (src (caddr instr)))
         (cond
          ((register? dst) (load! ctx (cadr dst) src))
          ((register? src) (store! ctx dst (cadr src)))
          ((number? (operand ctx src)) (emit-asm! ctx `(mov ,(slot ctx dst) ,(operand ctx src))))
          (else
           (load! ctx 'rax src)
           (store! ctx dst 'rax)))))

      ((stackcall)
       (let ((dst (cadr instr))
             (target (caddr instr))
             (args (cdddr instr)))
         (push-args! ctx args)
         (emit-asm! ctx `(call ,target))
         (unless (null? args)
           (emit-asm! ctx `(add rsp ,(* 8 (length args)))))
         (unless (eq? dst 'void)
           (store! ctx dst 'rax))))

      ;; The kernel clobbers rcx and r11, which nothing keeps live
      ((syscall)
       (let ((dst (cadr instr))
             (n (caddr instr))
             (args (cdddr instr)))
         (load-args! ctx args syscall-regs)
         (load! ctx 'eax n)
         (emit-asm! ctx '(syscall))
         (store! ctx dst 'rax)))

      ;; (call dst target frame): the caller allocated the callee's frame and stored the
      ;; arguments; target is a symbol, or frame for the code of the frame's self
      ((call)
       (let ((dst (cadr instr))
             (target (caddr instr))
             (frame (cadddr instr))
             (label (ret-label! ctx)))
         (enter-frame! ctx target frame)
         (emit-asm! ctx
                    `(lea r11 (rip ,label))
                    `(mov (mem rax ,frame-ret) r11)
                    `(mov (mem rax ,frame-caller) rbp)
                    '(mov rbp rax)
                    `(jmp ,(if (string? target) target 'rcx))
                    `(label ,label))
         (unless (eq? dst 'void)
           (store! ctx dst 'rax))))

      ;; The callee returns where this function would
      ((tailcall)
       (let ((target (cadr instr))
             (frame (caddr instr)))
         (enter-frame! ctx target frame)
         (emit-asm! ctx
                    `(mov r11 (mem rbp ,frame-ret))
                    `(mov (mem rax ,frame-ret) r11)
                    `(mov r11 (mem rbp ,frame-caller))
                    `(mov (mem rax ,frame-caller) r11)
                    '(mov rbp rax)
                    `(jmp ,(if (string? target) target 'rcx)))))

      ;; (selfcall proc arg ...): a tail call to this function's own code, in this frame;
      ;; the operands may be its own slots, so all are read before any is written
      ((selfcall)
       (let* ((ops (cdr instr))
              (slots (cons '(self) (map (lambda (i) (list 'arg (+ i 1))) (iota (length (cdr ops)))))))
         (if (<= (length ops) (length selfcall-regs))
             (let ((regs (list-head selfcall-regs (length ops))))
               (for-each (lambda (reg x) (load! ctx reg x)) regs ops)
               (for-each (lambda (x reg) (store! ctx x reg)) slots regs))
             (begin
               (for-each (lambda (x)
                           (load! ctx 'rax x)
                           (emit-asm! ctx '(push rax)))
                         ops)
               (for-each (lambda (x)
                           (emit-asm! ctx '(pop rax))
                           (store! ctx x 'rax))
                         (reverse slots))))
         (emit-asm! ctx `(jmp ,(body-label ctx)))))

      ((allocframe)
       (let ((dst (cadr instr))
             (nwords (caddr instr))
             (proc (cadddr instr)))
         (load! ctx 'rax nwords)
         (load! ctx 'rdx proc)
         (emit-asm! ctx '(call "alloc_frame"))
         (store! ctx dst 'rax)))

      ((alloc)
       (let ((dst (cadr instr))
             (nwords (caddr instr)))
         (load! ctx 'rax nwords)
         (emit-asm! ctx '(call "alloc_object"))
         (store! ctx dst 'rax)))

      ((store)
       (let ((base (cadr instr))
             (index (caddr instr))
             (src (cadddr instr)))
         (let ((src (if (pair? (operand ctx src))
                        (in-register! ctx src 'rdx)
                        (source! ctx src 'rdx))))
           (emit-asm! ctx `(mov ,(element! ctx base index) ,src)))))

      ;; A constant word of a symbol, such as a frame length, is one RIP-relative load
      ((load)
       (let ((dst (cadr instr))
             (base (caddr instr))
             (index (cadddr instr))
             (d (result-register (cadr instr))))
         (if (and (string? base) (eq? (car index) 'quad))
             (emit-asm! ctx `(mov ,d (rip ,base ,(* 8 (cadr index)))))
             (emit-asm! ctx `(mov ,d ,(element! ctx base index))))
         (store! ctx dst d)))

      ((store8)
       (let ((base (cadr instr))
             (index (caddr instr))
             (src (cadddr instr)))
         (if (and (eq? (car src) 'quad) (<= -128 (cadr src) 255))
             (emit-asm! ctx `(mov ,(byte-element! ctx base index 'mem8) ,(cadr src)))
             (begin
               (load! ctx 'rdx src)
               (emit-asm! ctx `(mov ,(byte-element! ctx base index 'mem8) dl))))))

      ((store32)
       (let ((base (cadr instr))
             (index (caddr instr))
             (src (cadddr instr)))
         (if (and (eq? (car src) 'quad) (imm32? (cadr src)))
             (emit-asm! ctx `(mov ,(byte-element! ctx base index 'mem32) ,(cadr src)))
             (begin
               (load! ctx 'rdx src)
               (emit-asm! ctx `(mov ,(byte-element! ctx base index 'mem32) edx))))))

      ((load8)
       (let ((dst (cadr instr))
             (base (caddr instr))
             (index (cadddr instr))
             (d (result-register (cadr instr))))
         (emit-asm! ctx `(movzx ,d ,(byte-element! ctx base index 'mem8)))
         (store! ctx dst d)))

      ((load32)
       (let ((dst (cadr instr))
             (base (caddr instr))
             (index (cadddr instr))
             (d (result-register (cadr instr))))
         (emit-asm! ctx `(mov ,(register32 d) ,(byte-element! ctx base index 'mem32)))
         (store! ctx dst d)))

      ((copy)
       (let ((dst (cadr instr))
             (src (caddr instr))
             (n (cadddr instr)))
         (load! ctx 'rdi dst)
         (load! ctx 'rsi src)
         (load! ctx 'rcx n)
         (emit-asm! ctx '(rep movsq))))

      ((fill)
       (let ((dst (cadr instr))
             (n (caddr instr))
             (x (cadddr instr)))
         (load! ctx 'rdi dst)
         (load! ctx 'rcx n)
         (load! ctx 'rax x)
         (emit-asm! ctx '(rep stosq))))

      ;; Not both in memory
      ((cmp)
       (let* ((b (source! ctx (caddr instr) 'rcx))
              (a (place! ctx (cadr instr) 'rax))
              (a (if (and (pair? a) (pair? b))
                     (begin
                       (load! ctx 'rax (cadr instr))
                       'rax)
                     a)))
         (emit-asm! ctx `(cmp ,a ,b))))

      ;; In the destination register when it has one, else in rax
      ((add sub mul and or xor)
       (let ((op (if (eq? (car instr) 'mul) 'imul (car instr)))
             (dst (cadr instr))
             (a (caddr instr))
             (b (cadddr instr)))
         (cond
          ((and (register? dst) (not (equal? b dst)))
           (arith! ctx op (cadr dst) a b))
          ((and (register? dst) (not (eq? op 'sub)))
           (arith! ctx op (cadr dst) b a))
          (else
           (arith! ctx op 'rax a b)
           (store! ctx dst 'rax)))))

      ((sal sar shr)
       (shift! ctx (car instr) (cadr instr) (caddr instr) (cadddr instr)))

      ;; The high 64 bits of the unsigned product: rdx:rax = rax * rcx
      ((mulhu)
       (let ((dst (cadr instr))
             (a (caddr instr))
             (b (cadddr instr)))
         (load! ctx 'rax a)
         (load! ctx 'rcx b)
         (emit-asm! ctx '(mul rcx))
         (store! ctx dst 'rdx)))

      ;; Signed, truncating toward zero
      ((div rem)
       (let ((dst (cadr instr))
             (a (caddr instr))
             (b (cadddr instr)))
         (load! ctx 'rax a)
         (load! ctx 'rcx b)
         (emit-asm! ctx
                    '(cqo)
                    '(idiv rcx))
         (store! ctx dst (if (eq? (car instr) 'div) 'rax 'rdx))))

      ;; Unsigned, rdx:rax / rcx, faulting unless rdx < rcx
      ((divwu)
       (let ((dst (cadr instr))
             (hi (caddr instr))
             (lo (cadddr instr))
             (d (car (cddddr instr))))
         (load! ctx 'rdx hi)
         (load! ctx 'rax lo)
         (load! ctx 'rcx d)
         (emit-asm! ctx '(div rcx))
         (store! ctx dst 'rax)))

      ((je jne jl jle jg jge jb jbe ja jae jmp)
       (let ((op (car instr))
             (label (cadr instr)))
         (emit-asm! ctx `(,op ,(label-name ctx label)))))

      ((unreachable)
       (emit-asm! ctx '(ud2)))

      ((ret)
       (let ((src (cadr instr)))
         (cond
          ((asm-context-noalloc? ctx)
           (unless (equal? src '(void))
             (load! ctx 'rax src))
           (emit-asm! ctx
                      '(leave)
                      '(ret)))
          (else
           (load! ctx 'rax src)
           (leave! ctx)
           (emit-asm! ctx '(jmp r11))))))

      (else
       (compile-error 'instr->asm! "unknown instruction" instr))))


  ;; x is (function|noalloc name (arity n) block ...);
  ;; a block is (block n instr ...), n being its label, and falls through to the next one
  (define (function->asm x)
    (let* ((head `(function ,(cadr x)))
           (blocks (dumped 'reg-alloc head (allocate-registers (cdddr x))))
           (ctx (new-asm-context x blocks))
           (fn (asm-context-fn ctx))
           (arity (asm-context-arity ctx))
           (nraw (asm-context-nraw ctx))
           (nwords (+ 6 arity (asm-context-ntmp ctx) nraw)))
      (if (asm-context-noalloc? ctx)
          (begin
            (emit-asm! ctx
                       '(text)
                       `(label ,fn)
                       '(push rbp)
                       '(mov rbp rsp))
            (unless (zero? (+ (asm-context-ntmp ctx) nraw))
              (emit-asm! ctx `(sub rsp ,(* 8 (+ (asm-context-ntmp ctx) nraw))))))
          ;; The frame length, which callers read before the code
          (emit-asm! ctx
                     '(text)
                     '(balign 8)
                     `(quad ,nwords)
                     `(label ,fn)
                     `(mov (mem rbp ,frame-nraw) ,nraw)
                     `(label ,(body-label ctx))))
      (for-each (lambda (block)
                  (emit-asm! ctx `(label ,(label-name ctx (cadr block))))
                  (for-each (lambda (instr) (instr->asm! ctx instr)) (cddr block)))
                blocks)
      (dumped 'peephole head (peephole (dumped 'asm head (reverse (asm-context-code ctx)))))))


  (define (top->asm x)
    (case (car x)
      ;; (rodata name (quad x) ...): read-only words, which may hold relocated addresses
      ((rodata)
       `((section ".data.rel.ro")
         (balign 8)
         (label ,(cadr x))
         (quad ,@(map cadr (cddr x)))))
      ((string)
       `((section ".rodata")
         (label ,(cadr x))
         (asciz ,(caddr x))))
      ;; (data name n word): n words, each initialized to word
      ((data)
       (let ((n (caddr x)))
         `((section ".data")
           (balign 8)
           (label ,(cadr x))
           ,@(if (= n 0)
                 '()
                 (list (cons 'quad (make-list n (cadddr x))))))))
      ((function noalloc)
       (function->asm x))
      ((stubs) runtime-asm)
      (else (compile-error 'top->asm "unknown top-level entry" x))))


  ;; The process entry and the allocation stub, in the library
  (define runtime-asm
    `((text)
      ;; The process entry: rsp points to argc; it allocates the frame
      ;; of scheme_main, which runs in the Scheme calling convention, and nothing returns here
      (globl "_start")
      (label "_start")
      (push rsp)
      (call "init_runtime_code")
      (xor ebp ebp)
      (mov rax (rip "scheme_main" -8))
      (xor edx edx)
      (call "alloc_frame")
      (lea r11 (rip ".L_start_finish"))
      (mov (mem rax ,frame-ret) r11)
      (mov rbp rax)
      (jmp "scheme_main")
      (label ".L_start_finish")
      (push rax)
      (call "finish_program_code")

      ;; Allocate an object, zeroed
      ;; Parameters:
      ;;   - rax: length (in words) of the object
      ;;   - rbp: current frame, the only root
      ;; Output:
      ;;   - rax: the object
      ;;   - rbp: the current frame, moved if a collection happened
      ;; Clobbers: all other registers
      ;; The fast path bumps scheme_len up to scheme_limit (0 in stress mode)
      (globl "alloc_object")
      (label "alloc_object")
      (mov rcx (rip "scheme_len"))
      (add rcx rax)
      (cmp rcx (rip "scheme_limit"))
      (ja ".Lalloc_object_slow")
      (mov (rip "scheme_len") rcx)
      (sub rcx rax)
      (shl rcx 3)
      (add rcx (rip "scheme_mem"))
      (mov rax rcx)
      (ret)
      (label ".Lalloc_object_slow")
      (push rbp)                        ; roots[0]
      (mov rcx rsp)
      (push 1)
      (push rcx)
      (push rax)
      (call "alloc_words_code")
      (add rsp 24)
      (pop rbp)
      (ret)

      ;; Allocate a frame, zeroed but its header and self
      ;; Parameters:
      ;;   - rax: length (in words) of the frame
      ;;   - rdx: procedure object, the frame's self, a root
      ;;   - rbp: current frame, a root
      ;; Output:
      ;;   - rax: the frame
      ;;   - rbp: the current frame, moved if a collection happened
      ;; Clobbers: all other registers
      (globl "alloc_frame")
      (label "alloc_frame")
      (mov rcx (rip "scheme_len"))
      (add rcx rax)
      (cmp rcx (rip "scheme_limit"))
      (ja ".Lalloc_frame_slow")
      (mov (rip "scheme_len") rcx)
      (sub rcx rax)
      (shl rcx 3)
      (add rcx (rip "scheme_mem"))
      (label ".Lalloc_frame_init")
      (mov (mem rcx 0) ,frame-type)
      (mov (mem rcx 8) rax)
      (mov (mem rcx ,frame-self) rdx)
      (mov rax rcx)
      (ret)
      (label ".Lalloc_frame_slow")
      (push rax)                        ; saved, not a root
      (push rdx)                        ; roots: rbp, rdx
      (push rbp)
      (mov rcx rsp)
      (push 2)
      (push rcx)
      (push rax)
      (call "alloc_words_code")
      (add rsp 24)
      (mov rcx rax)
      (pop rbp)
      (pop rdx)
      (pop rax)
      (jmp ".Lalloc_frame_init")))


  (define (program->asm tops exports)
    (append '((intel_syntax noprefix))
            (map (lambda (name) `(globl ,name)) exports)
            (apply append (map top->asm tops))
            '((section ".note.GNU-stack" "" %progbits))))


  (define *directives* '(intel_syntax globl text section balign quad asciz))


  ;; Writes s as a GNU as string: printable ASCII as is, other bytes in octal
  (define (write-gas-string s)
    (write-char #\")
    (for-each (lambda (b)
                (cond
                 ((memv b '(34 92)) (printf "\\~a" (integer->char b)))
                 ((<= 32 b 126) (write-char (integer->char b)))
                 ;; Always 3 digits, so the escape ends there
                 (else
                  (write-char #\\)
                  (for-each (lambda (d) (write-char (integer->char (+ 48 d))))
                            (list (quotient b 64) (modulo (quotient b 8) 8) (modulo b 8))))))
              (bytevector->u8-list (string->utf8 s)))
    (write-char #\"))


  (define (print-operand x)
    (cond
     ((or (symbol? x) (number? x))
      (display x))
     ((string? x)
      (write-gas-string x))
     ((and (eq? (car x) 'mem) (= (length x) 4))
      (printf "qword ptr [~a + ~a*~a]" (cadr x) (caddr x) (cadddr x)))
     ((memq (car x) '(mem mem8 mem32))
      (let ((size (case (car x) ((mem) "qword") ((mem8) "byte") (else "dword")))
            (off (caddr x)))
        (if (symbol? off)
            (printf "~a ptr [~a + ~a]" size (cadr x) off)
            (printf "~a ptr [~a ~a ~a]" size (cadr x) (if (< off 0) "-" "+") (abs off)))))
     ((and (eq? (car x) 'rip) (pair? (cddr x)))
      (printf "qword ptr [rip + ~s ~a ~a]" (cadr x) (if (< (caddr x) 0) "-" "+") (abs (caddr x))))
     ((eq? (car x) 'rip)
      (printf "qword ptr [rip + ~s]" (cadr x)))
     (else (compile-error 'print-operand "unknown operand" x))))


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


  (define (compile-asm tops)
    (let-values (((exports tops) (partition (lambda (x) (eq? (car x) 'export)) tops)))
      (for-each print-asm (dumped 'program '(program) (program->asm tops (apply append (map cdr exports))))))))
