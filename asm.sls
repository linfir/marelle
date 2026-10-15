;; Frames are heap objects, rbp pointing to the current one, n being the
;; function's arity and m its number of tmps:
;;
;;   rbp + 0            frame type (43 << 3)
;;   rbp + 8            length of the frame in words, header included (raw)
;;   rbp + 16           number of raw slots at the end, not scanned (raw)
;;   rbp + 24           return address (raw)
;;   rbp + 32           caller's frame, or 0 for scheme_main
;;   rbp + 40           (self), the procedure object passed in r10, or 0 for scheme_main
;;   rbp + 48           (arg 1)
;;   ...
;;   rbp + 8*(n+5)      (arg n)
;;   rbp + 8*(n+6)      (tmp 1)
;;   ...
;;   rbp + 8*(n+m+5)    (tmp m)
;;   rbp + 8*(n+m+6)    (raw 1)
;;   ...
;;
;; Native functions have their frame on the machine stack, rsp pointing to it:
;;
;;   rsp + 0            (arg 1)
;;   ...                then the tmps and the raws, as above
;;
;; C calls (ccall) follow the System V AMD64 ABI:
;;   - arguments in rdi, rsi, rdx, rcx, r8, r9, result in rax
;;   - al = 0 (no vector arguments), for variadic functions
;;   - rbx, rbp, rsp, r12-r15 are preserved, all other registers are clobbered
;;   - rsp is 16-byte aligned at the call; Scheme code never pushes, and
;;     scheme_enter aligns it once, so a plain call is always aligned
;;
;; Scheme calls (call, tailcall) use their own convention:
;;   - arguments in rdi, rsi, rdx, rcx, r8, r9 (at most 6), the procedure object
;;     in r10, the return address in r11, then a jmp to the code (no push)
;;   - the callee's prologue calls alloc_frame, which saves rbp, r10 and r11 in
;;     the new frame; the number of raw slots and the arguments are then
;;     stored in it
;;   - ret puts the result in rax, restores the caller's rbp and jumps to the
;;     saved return address
;;   - a tail call passes its own caller's frame and return address, so the
;;     callee returns directly there
;;   - a tail call to the function itself (known, or the code pointer compared
;;     at runtime) reuses the frame: it stores r10 as self and jumps to the
;;     prologue's argument stores
;;   - only rbp and rsp are preserved: all values live in frames
;;   - the arity is checked by the caller, not the callee

(library (asm)
  (export compile-asm)
  (import (chezscheme)
          (utils))


  (define (ir-entries tag tops)
    (filter (lambda (x) (eq? (car x) tag)) tops))


  (define arg-regs '(rdi rsi rdx rcx r8 r9))


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
    (fields fn native? arity ntmp nraw (mutable code) ret-counter))


  ;; x is (function name (arity n) block ...) or (native name (arity n) block ...),
  ;; a block being (block n instr ...),
  ;; n being its label,
  ;; and falling through to the next one
  (define (new-asm-context x)
    (let ((blocks (cdddr x)))
      (make-asm-context (cadr x) (eq? (car x) 'native) (cadr (caddr x))
                        (count-slots 'tmp blocks) (count-slots 'raw blocks)
                        '() (make-counter))))


  (define (emit-asm! ctx . instrs)
    (asm-context-code-set! ctx (append (reverse instrs) (asm-context-code ctx))))


  ;; Word i of the frame after the header, from 1: the args, the tmps, then the raws
  (define (frame-word ctx i)
    (if (asm-context-native? ctx)
        `(mem rsp ,(* 8 (- i 1)))
        `(mem rbp ,(+ frame-self (* 8 i)))))


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
       (error 'slot "not a slot operand" x))))


  ;; The bytes of a native frame, keeping rsp 16-byte aligned at calls:
  ;; it is 8 past a multiple of 16 on entry, the return address being pushed
  (define (native-bytes ctx)
    (let ((n (+ (asm-context-arity ctx) (asm-context-ntmp ctx) (asm-context-nraw ctx))))
      (* 8 (if (even? n) (+ n 1) n))))


  (define (load! ctx reg x)
    (cond
     ((string? x)
      (emit-asm! ctx `(lea ,reg (rip ,x))))
     ((pair? x)
      (case (car x)
        ((self tmp raw arg)
         (emit-asm! ctx `(mov ,reg ,(slot ctx x))))
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
    (string-append ".L" (asm-context-fn ctx) "_" (number->string label)))


  (define (ret-label! ctx)
    (string-append ".L" (asm-context-fn ctx) "_ret"
                   (number->string ((asm-context-ret-counter ctx)))))


  (define (self-label ctx)
    (string-append ".L" (asm-context-fn ctx) "_self"))


  ;; The memory operand for word index of the address in rax, using rcx
  (define (element! ctx index)
    (case (car index)
      ((quad)
       `(mem rax ,(* 8 (cadr index))))
      (else
       (load! ctx 'rcx index)
       '(mem rax rcx 8))))


  (define (leave! ctx)
    (emit-asm! ctx
               `(mov r11 (mem rbp ,frame-ret))
               `(mov rbp (mem rbp ,frame-caller))))


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
         (emit-asm! ctx
                    '(xor eax eax)
                    `(call ,target))
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
         (unless (eq? dst 'void)
           (store! ctx dst 'rax))))

      ((tailcall)
       (let ((target (cadr instr))
             (proc (caddr instr))
             (args (cdddr instr)))
         (load-args! ctx args)
         (load! ctx 'r10 proc)
         (cond
          ((equal? target (asm-context-fn ctx))
           (emit-asm! ctx
                      `(mov (mem rbp ,frame-self) r10)
                      `(jmp ,(self-label ctx))))
          (else
           (load! ctx 'rax target)
           (unless (string? target)
             ;; The frame is dead if the jump is not taken
             (emit-asm! ctx
                        `(mov (mem rbp ,frame-self) r10)
                        `(lea r11 (rip ,(asm-context-fn ctx)))
                        '(cmp rax r11)
                        `(je ,(self-label ctx))))
           (leave! ctx)
           (emit-asm! ctx '(jmp rax))))))

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
         (load! ctx 'rax base)
         (load! ctx 'rdx src)
         (emit-asm! ctx `(mov ,(element! ctx index) rdx))))

      ((load)
       (let ((dst (cadr instr))
             (base (caddr instr))
             (index (cadddr instr)))
         (load! ctx 'rax base)
         (emit-asm! ctx `(mov rax ,(element! ctx index)))
         (store! ctx dst 'rax)))

      ((cmp)
       (let ((a (cadr instr))
             (b (caddr instr)))
         (load! ctx 'rax a)
         (load! ctx 'rcx b)
         (emit-asm! ctx '(cmp rax rcx))))

      ((add sub mul sal sar and or)
       (let ((op (cdr (assq (car instr) '((add . add) (sub . sub) (mul . imul)
                                          (sal . sal) (sar . sar) (and . and) (or . or)))))
             (dst (cadr instr))
             (a (caddr instr))
             (b (cadddr instr)))
         (load! ctx 'rax a)
         (load! ctx 'rcx b)
         (emit-asm! ctx `(,op rax ,(if (memq op '(sal sar)) 'cl 'rcx)))
         (store! ctx dst 'rax)))

      ((je jne jl jle jg jge jmp)
       (let ((op (car instr))
             (label (cadr instr)))
         (emit-asm! ctx `(,op ,(label-name ctx label)))))

      ((unreachable)
       (emit-asm! ctx '(ud2)))

      ((ret)
       (let ((src (cadr instr)))
         (cond
          ((asm-context-native? ctx)
           (unless (equal? src '(void))
             (load! ctx 'rax src))
           (emit-asm! ctx
                      `(add rsp ,(native-bytes ctx))
                      '(ret)))
          (else
           (load! ctx 'rax src)
           (leave! ctx)
           (emit-asm! ctx '(jmp r11))))))

      (else
       (error 'instr->asm! "unknown instruction" instr))))


  ;; x is (function|native name (arity n) block ...), a block being
  ;; (block n instr ...), n being its label, and falling through to the next one
  (define (function->asm x)
    (let* ((ctx (new-asm-context x))
           (fn (asm-context-fn ctx))
           (arity (asm-context-arity ctx))
           (nraw (asm-context-nraw ctx))
           (nwords (+ 6 arity (asm-context-ntmp ctx) nraw)))
      (when (> arity (length arg-regs))
        (error 'function->asm "too many parameters" fn))
      (emit-asm! ctx
                 '(text)
                 `(label ,fn))
      (if (asm-context-native? ctx)
          (emit-asm! ctx `(sub rsp ,(native-bytes ctx)))
          (emit-asm! ctx
                     `(mov rax ,nwords)
                     '(call "alloc_frame")
                     `(mov (mem rbp ,frame-nraw) ,nraw)
                     `(label ,(self-label ctx))))
      (let loop ((i 1)
                 (regs arg-regs))
        (when (<= i arity)
          (store! ctx (list 'arg i) (car regs))
          (loop (+ i 1) (cdr regs))))
      (for-each (lambda (block)
                  (emit-asm! ctx `(label ,(label-name ctx (cadr block))))
                  (for-each (lambda (instr) (instr->asm! ctx instr)) (cddr block)))
                (cdddr x))
      (reverse (asm-context-code ctx))))


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
      ((function native)
       (function->asm x))
      (else (error 'top->asm "unknown top-level entry" x))))


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
                 (else (printf "\\~3,'0o" b))))
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


  (define (expand-top x)
    (case (car x)
      ((export) '())
      (else (list x))))


  (define (compile-asm tops)
    (for-each print-asm
              (program->asm (apply append (map expand-top tops))
                            (apply append (map cdr (ir-entries 'export tops)))))))
