;; A peephole pass on the asm of one function, for what asm cannot see one
;; instruction at a time:
;;
;; - a jmp to a label that follows it, with only labels in between, goes;
;; - (jcc a) (jmp b) (label a) becomes (jncc b) (label a);
;; - a store to a frame slot already holding the same register goes. The
;;   saves around calls store a register back to the slot it was restored
;;   from a few instructions earlier.
;;
;; For the stores, the pass knows which slots ([rbp + k]) hold the same
;; value as which register, within straight-line code: a label, a jump, a
;; call or an instruction it does not follow forgets everything, and so does
;; a store through another base register, which might alias the frame.

(library (peephole)
  (export peephole)
  (import (chezscheme))


  (define inverse-jumps
    '((je . jne) (jne . je) (jl . jge) (jge . jl) (jle . jg) (jg . jle)
      (jb . jae) (jae . jb) (jbe . ja) (ja . jbe)))


  ;; The 64-bit register of a register operand, else #f
  (define (register x)
    (case x
      ((eax) 'rax)
      ((ebx) 'rbx)
      ((cl ecx) 'rcx)
      ((dl edx) 'rdx)
      ((esi) 'rsi)
      ((edi) 'rdi)
      ((r8d) 'r8)
      ((r9d) 'r9)
      ((r10d) 'r10)
      ((r11d) 'r11)
      ((r12d) 'r12)
      ((r13d) 'r13)
      ((r14d) 'r14)
      ((r15d) 'r15)
      (else (and (symbol? x) x))))


  (define (frame-slot? x)
    (and (pair? x) (eq? (car x) 'mem) (eq? (cadr x) 'rbp) (null? (cdddr x))))


  (define (jumps x)
    (let loop ((x x))
      (cond
       ((null? x) '())
       ((and (eq? (caar x) 'jmp) (string? (cadar x)) (label-follows? (cadar x) (cdr x)))
        (loop (cdr x)))
       ((and (assq (caar x) inverse-jumps)
             (pair? (cdr x))
             (eq? (caadr x) 'jmp)
             (string? (cadadr x))
             (pair? (cddr x))
             (equal? (caddr x) `(label ,(cadar x))))
        (cons `(,(cdr (assq (caar x) inverse-jumps)) ,(cadadr x)) (loop (cddr x))))
       (else
        (cons (car x) (loop (cdr x)))))))


  (define (label-follows? label x)
    (and (pair? x)
         (eq? (caar x) 'label)
         (or (equal? (cadar x) label) (label-follows? label (cdr x)))))


  ;; known: ((slot . register) ...), each slot holding the value of its register
  (define (stores x)
    (let loop ((x x) (known '()) (acc '()))
      (if (null? x)
          (reverse acc)
          (let* ((instr (car x))
                 (op (car instr))
                 (dst (and (pair? (cdr instr)) (cadr instr)))
                 (src (and dst (pair? (cddr instr)) (caddr instr))))
            (define (forget reg)
              (filter (lambda (k) (not (eq? (cdr k) reg))) known))
            (cond
             ((and (eq? op 'mov) (frame-slot? dst) (symbol? src))
              (if (equal? (assoc dst known) (cons dst src))
                  (loop (cdr x) known acc)
                  (loop (cdr x)
                        (cons (cons dst src) (filter (lambda (k) (not (equal? (car k) dst))) known))
                        (cons instr acc))))
             ((and (eq? op 'mov) (symbol? dst) (not (eq? dst 'rbp)) (frame-slot? src))
              (loop (cdr x)
                    (cons (cons src dst) (filter (lambda (k) (not (equal? (car k) src))) (forget dst)))
                    (cons instr acc)))
             ((and (eq? op 'mov) (frame-slot? dst))
              (loop (cdr x) (filter (lambda (k) (not (equal? (car k) dst))) known) (cons instr acc)))
             ((eq? op 'cmp)
              (loop (cdr x) known (cons instr acc)))
             ((and (memq op '(mov movzx lea add sub imul and or sal sar shr))
                   (register dst)
                   (not (eq? (register dst) 'rbp)))
              (loop (cdr x) (forget (register dst)) (cons instr acc)))
             (else
              (loop (cdr x) '() (cons instr acc))))))))


  (define (peephole x)
    (stores (jumps x))))
