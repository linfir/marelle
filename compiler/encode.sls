;; The x86-64 encoder: the asm s-expressions of asm.sls to a unit,
;; its sections' bytes, its labels and its fixups, the references left to resolve
;; (object.sls writes a unit as an ELF object).
;;
;; The encoding is GNU as's, byte for byte:
;; - jumps to a label of the same section, even a global one,
;;   start short (rel8) and grow to rel32 until all fit (relaxation);
;; - disp8 when the displacement fits, none for 0 (but from rbp or r13);
;; - sign-extended imm8 when it fits, the short rax form for imm32,
;;   movabs only for a 64-bit immediate,
;;   immediates taken modulo 2^64 (so 2^64 - 1 is -1);
;; - .balign in .text pads with GNU as's NOPs, zeros elsewhere.
;;
;; Fixups: abs64 (a quad), pc32 (a RIP-relative operand), plt32 (a call to a symbol),
;; jmp32 (a jmp to a symbol of another section);
;; the field holds symbol + addend, less its own address but for abs64.
;;
;; A section's bytes are encoded into one buffer as they come;
;; only its labels, alignments and relaxable jumps are items, at their position in the buffer,
;; so relaxation works on them alone.

(library (encode)
  (export encode-asm s32?
          unit-sections unit-labels unit-globals unit-fixups
          section-name section-align section-bytes
          unit-label-name unit-label-section unit-label-offset
          fixup-section fixup-offset fixup-type fixup-symbol fixup-addend)
  (import (chezscheme)
          (bytes))


  ;; x86-64 instruction encoding
  ;;
  ;; An instruction is, in order: prefixes (#xf3 for rep, #x66 in some NOPs), a
  ;; REX prefix, the opcode (1 byte, or 2 starting with #x0f), a ModRM byte, a
  ;; SIB byte, a displacement (1 or 4 bytes) and an immediate (1, 4 or 8
  ;; bytes), all but the opcode optional.
  ;; Multi-byte fields are little-endian.
  ;; Registers are numbered from 0 to 15: ModRM and SIB hold the low 3 bits of
  ;; a number, the REX prefix the fourth.


  ;; The size of register x in bits, or #f.
  ;; The 8-bit ones are only those encoded the same with or without a REX prefix: 4 to 7 are
  ;; ah to bh without one, spl to dil with one.
  (define (reg-size x)
    (case x
      ((rax rcx rdx rbx rsp rbp rsi rdi r8 r9 r10 r11 r12 r13 r14 r15) 64)
      ((eax ecx edx ebx esp ebp esi edi r8d r9d r10d r11d r12d r13d r14d r15d) 32)
      ((al cl dl bl) 8)
      (else #f)))


  (define (reg-num r)
    (case r
      ((rax eax al) 0) ((rcx ecx cl) 1) ((rdx edx dl) 2) ((rbx ebx bl) 3)
      ((rsp esp) 4) ((rbp ebp) 5) ((rsi esi) 6) ((rdi edi) 7)
      ((r8 r8d) 8) ((r9 r9d) 9) ((r10 r10d) 10) ((r11 r11d) 11)
      ((r12 r12d) 12) ((r13 r13d) 13) ((r14 r14d) 14) ((r15 r15d) 15)
      (else (error 'encode-asm "not a register" r))))


  (define (reg? x)
    (and (reg-size x) #t))


  (define (mem? x)
    (and (pair? x) (memq (car x) '(mem mem8 mem32 rip)) #t))


  (define (s8? n)
    (and (>= n -128) (<= n 127)))


  (define (s32? n)
    (and (>= n -2147483648) (<= n 2147483647)))


  ;; n modulo 2^64, signed; a fixnum already is
  (define (signed64 n)
    (if (fixnum? n)
        n
        (let ((n (logand n 18446744073709551615)))
          (if (>= n 9223372036854775808) (- n 18446744073709551616) n))))


  ;; A unit: sections in order of first use, labels and fixups by section, then offset, globals
  ;; in order of declaration
  (define-record-type unit
    (fields sections labels globals fixups))


  (define-record-type section
    (fields name align bytes))


  ;; section an index in the unit's sections
  (define-record-type unit-label
    (fields name section offset))


  (define-record-type fixup
    (fields section (mutable offset) type symbol addend))


  ;; An item of a section: a label (arg its name), an alignment (arg the alignment) or a
  ;; relaxable jump to a local label (arg its name, cc the condition code or #f for jmp; target
  ;; the index of the label's item), at pos in the section's buffer
  (define-record-type item
    (fields kind pos arg cc (mutable long?) (mutable target)))


  ;; A section being encoded: its buffer, items and fixups newest first, largest alignment
  (define-record-type section-state
    (fields name index buffer (mutable items) (mutable fixups) (mutable align)))


  (define (add-item! s kind arg cc)
    (section-state-items-set!
     s (cons (make-item kind (buffer-size (section-state-buffer s)) arg cc #f #f)
             (section-state-items s))))


  ;; A fixup at offset bytes from the end of the buffer
  (define (add-fixup! s offset type symbol addend)
    (section-state-fixups-set!
     s (cons (make-fixup (section-state-index s) (+ (buffer-size (section-state-buffer s)) offset)
                         type symbol addend)
             (section-state-fixups s))))


  ;; The REX prefix, #x40 to #x4f, of the bits W (8) for a 64-bit operation, then
  ;; the fourth bit of R (4) the ModRM reg, X (2) the SIB index and B (1) the
  ;; ModRM rm or SIB base.
  ;; It is left out when no bit is set.
  (define (rex! b w r x base)
    (let ((bits (logior (if w 8 0)
                        (if (>= r 8) 4 0)
                        (if (>= x 8) 2 0)
                        (if (>= base 8) 1 0))))
      (unless (= bits 0)
        (buffer-u8! b (logior #x40 bits)))))


  ;; The ModRM byte: mod (2 bits), reg (3), rm (3).
  ;; reg is a register operand or, for an opcode of one operand, an extension of
  ;; the opcode (the /n of the manuals).
  ;; With mod 3, rm is a register; otherwise it is the base of a memory operand
  ;; followed by no displacement (mod 0), a disp8 (1) or a disp32 (2).
  ;; Two rm values are escapes: 4 for a SIB byte, so rsp and r12 as a base need one;
  ;; 5 with mod 0 for RIP-relative, so rbp and r13 as a base need a displacement.
  (define (modrm mod reg rm)
    (+ (* mod 64) (* (logand reg 7) 8) (logand rm 7)))


  ;; The index and base of the operand x for the REX prefix
  (define (operand-index x)
    (if (and (pair? x) (not (eq? (car x) 'rip)) (pair? (cddr x)) (symbol? (caddr x)))
        (reg-num (caddr x))
        0))


  (define (operand-base x)
    (cond
     ((reg? x) (reg-num x))
     ((eq? (car x) 'rip) 0)
     (else (reg-num (cadr x)))))


  ;; The ModRM and what follows of the operand x, reg being the reg field; imm-size the bytes
  ;; of the immediate after it, since a RIP-relative disp32 is from the end of the instruction.
  ;; The SIB byte: scale (2 bits, log2 of 1, 2, 4 or 8), index (3), base (3);
  ;; index 4 is none, so #x24 is rsp alone as the base.
  (define (rm! s reg x imm-size)
    (let ((b (section-state-buffer s)))
      (cond
       ((reg? x)
        (buffer-u8! b (modrm 3 reg (reg-num x))))
       ((eq? (car x) 'rip)
        (buffer-u8! b (modrm 0 reg 5))
        (add-fixup! s 0 'pc32 (cadr x) (- (if (pair? (cddr x)) (caddr x) 0) 4 imm-size))
        (buffer-le! b 0 4))
       (else
        (let* ((base (reg-num (cadr x)))
               (index (and (pair? (cddr x)) (symbol? (caddr x)) (reg-num (caddr x))))
               (scale (if (pair? (cdddr x)) (cadddr x) 1))
               (disp (if (or index (null? (cddr x))) 0 (caddr x)))
               (mod (cond
                     ((and (= disp 0) (not (= (logand base 7) 5))) 0)
                     ((s8? disp) 1)
                     ((s32? disp) 2)
                     (else (error 'encode-asm "displacement too large" x)))))
          (cond
           (index
            (buffer-u8! b (modrm mod reg 4))
            (buffer-u8! b (+ (* 64 (case scale ((1) 0) ((2) 1) ((4) 2) (else 3)))
                             (* 8 (logand index 7))
                             (logand base 7))))
           ((= (logand base 7) 4)
            (buffer-u8! b (modrm mod reg 4))
            (buffer-u8! b #x24))
           (else
            (buffer-u8! b (modrm mod reg base))))
          (case mod
            ((1) (buffer-le! b disp 1))
            ((2) (buffer-le! b disp 4))))))))


  ;; prefixes, REX, opcode, the operand x with reg in the reg field, then the immediate imm of
  ;; imm-size bytes; w for a 64-bit operation
  (define (op-rm! s prefixes w opcode reg x imm imm-size)
    (let ((b (section-state-buffer s)))
      (for-each (lambda (p) (buffer-u8! b p)) prefixes)
      (rex! b w reg (operand-index x) (operand-base x))
      (for-each (lambda (o) (buffer-u8! b o)) opcode)
      (rm! s reg x imm-size)
      (unless (= imm-size 0)
        (buffer-le! b imm imm-size))))


  (define (bytes! s . bytes)
    (let ((b (section-state-buffer s)))
      (for-each (lambda (x) (buffer-u8! b x)) bytes)))


  (define (wide? x)
    (or (eqv? (reg-size x) 64) (and (pair? x) (memq (car x) '(mem rip)) #t)))


  ;; #x89 is mov r/m, reg and #x8b mov reg, r/m (GNU as picks #x89 between
  ;; registers); #x88 is #x89 on bytes.
  ;; #xc7 /0 stores a sign-extended imm32 and #xc6 /0 an imm8.
  ;; #xb8 + r, the register in the opcode, loads an imm64 with REX.W (movabs),
  ;; an imm32 without, zero-extended.
  (define (encode-mov! s dst src)
    (let ((b (section-state-buffer s)))
      (cond
       ((and (reg? dst) (reg? src))
        (op-rm! s '() (wide? dst) '(#x89) (reg-num src) dst 0 0))
       ((and (eqv? (reg-size dst) 64) (integer? src))
        (if (s32? src)
            (op-rm! s '() #t '(#xc7) 0 dst src 4)
            (let ((r (reg-num dst)))
              (rex! b #t 0 0 r)
              (buffer-u8! b (+ #xb8 (logand r 7)))
              (buffer-le! b src 8))))
       ((and (eqv? (reg-size dst) 32) (integer? src))
        (let ((r (reg-num dst)))
          (rex! b #f 0 0 r)
          (buffer-u8! b (+ #xb8 (logand r 7)))
          (buffer-le! b src 4)))
       ((and (reg? dst) (mem? src))
        (op-rm! s '() (wide? dst) '(#x8b) (reg-num dst) src 0 0))
       ((and (mem? dst) (eqv? (reg-size src) 8))
        (op-rm! s '() #f '(#x88) (reg-num src) dst 0 0))
       ((and (mem? dst) (reg? src))
        (op-rm! s '() (wide? src) '(#x89) (reg-num src) dst 0 0))
       ((and (mem? dst) (eq? (car dst) 'mem8) (integer? src))
        (op-rm! s '() #f '(#xc6) 0 dst src 1))
       ((and (mem? dst) (integer? src) (s32? src))
        (op-rm! s '() (wide? dst) '(#xc7) 0 dst src 4))
       (else
        (error 'encode-asm "unsupported mov" dst src)))))


  ;; Group 1 operation n (add 0, or 1, and 4, sub 5, xor 6, cmp 7).
  ;; Its opcodes: 8n + 1 op r/m, reg; 8n + 3 op reg, r/m; 8n + 5 op rax, imm32,
  ;; without ModRM; #x81 /n with an imm32 and #x83 /n with a sign-extended imm8.
  (define (encode-alu! s n dst src)
    (cond
     ((and (or (reg? dst) (mem? dst)) (reg? src))
      (op-rm! s '() (wide? dst) (list (+ (* n 8) 1)) (reg-num src) dst 0 0))
     ((and (reg? dst) (mem? src))
      (op-rm! s '() #t (list (+ (* n 8) 3)) (reg-num dst) src 0 0))
     ((and (integer? src) (s8? src))
      (op-rm! s '() (wide? dst) '(#x83) n dst src 1))
     ((and (integer? src) (s32? src) (reg? dst) (= (reg-num dst) 0))
      (let ((b (section-state-buffer s)))
        (rex! b (wide? dst) 0 0 0)
        (buffer-u8! b (+ (* n 8) 5))
        (buffer-le! b src 4)))
     ((and (integer? src) (s32? src))
      (op-rm! s '() (wide? dst) '(#x81) n dst src 4))
     (else
      (error 'encode-asm "unsupported operands" n dst src))))


  ;; Shift n (shl/sal 4, shr 5, sar 7) by cl or an immediate: #xd3 /n by cl,
  ;; #xd1 /n by 1, #xc1 /n by an imm8
  (define (encode-shift! s n dst count)
    (cond
     ((eq? count 'cl) (op-rm! s '() (wide? dst) '(#xd3) n dst 0 0))
     ((eqv? count 1) (op-rm! s '() (wide? dst) '(#xd1) n dst 0 0))
     ((integer? count) (op-rm! s '() (wide? dst) '(#xc1) n dst count 1))
     (else (error 'encode-asm "unsupported shift count" count))))


  ;; The low 4 bits of the opcodes of the conditional jumps: #x70 + cc with a
  ;; rel8, #x0f #x80 + cc with a rel32.
  ;; A short jmp is #xeb rel8, a long one #xe9 rel32; rel is from the end of the jump.
  (define condition-codes
    '((jb . 2) (jae . 3) (je . 4) (jne . 5) (jbe . 6) (ja . 7)
      (jl . 12) (jge . 13) (jle . 14) (jg . 15)))


  ;; A call or a jmp: through a register, a relaxable jump to a local label, or a
  ;; rel32 to a symbol, resolved later if it is local.
  ;; #xff /2 is call r/m and #xff /4 jmp r/m; #xe8 is call rel32 and #xe9 jmp
  ;; rel32, from the end of the instruction (hence the addend -4).
  (define (encode-transfer! s op target local?)
    (cond
     ((reg? target)
      (op-rm! s '() #f '(#xff) (if (eq? op 'call) 2 4) target 0 0))
     ((and (eq? op 'jmp) (local? target))
      (add-item! s 'jump target #f))
     (else
      (bytes! s (if (eq? op 'call) #xe8 #xe9))
      (add-fixup! s 0 (if (eq? op 'call) 'plt32 'jmp32) target -4)
      (buffer-le! (section-state-buffer s) 0 4))))


  ;; local? tells whether a symbol is a label of the current section.
  ;; The other opcodes: movzx #x0f #xb6 (from a byte), lea #x8d, imul #x0f #xaf
  ;; reg, r/m and #x6b or #x69 reg, r/m, imm8 or imm32; mul, div and idiv #xf7
  ;; /4, /6 and /7; push and pop #x50 and #x58 + r, push #xff /6 r/m, #x6a imm8
  ;; and #x68 imm32.
  (define (encode! s instr local?)
    (let ((op (car instr))
          (args (map (lambda (x) (if (integer? x) (signed64 x) x)) (cdr instr))))
      (case op
        ((mov) (encode-mov! s (car args) (cadr args)))
        ((movzx) (op-rm! s '() (wide? (car args)) '(#x0f #xb6) (reg-num (car args)) (cadr args) 0 0))
        ((lea) (op-rm! s '() #t '(#x8d) (reg-num (car args)) (cadr args) 0 0))
        ((add) (encode-alu! s 0 (car args) (cadr args)))
        ((or) (encode-alu! s 1 (car args) (cadr args)))
        ((and) (encode-alu! s 4 (car args) (cadr args)))
        ((sub) (encode-alu! s 5 (car args) (cadr args)))
        ((xor) (encode-alu! s 6 (car args) (cadr args)))
        ((cmp) (encode-alu! s 7 (car args) (cadr args)))
        ((imul)
         (let ((reg (reg-num (car args))))
           (cond
            ((null? (cddr args)) (op-rm! s '() #t '(#x0f #xaf) reg (cadr args) 0 0))
            ((s8? (caddr args)) (op-rm! s '() #t '(#x6b) reg (cadr args) (caddr args) 1))
            (else (op-rm! s '() #t '(#x69) reg (cadr args) (caddr args) 4)))))
        ((sal shl) (encode-shift! s 4 (car args) (cadr args)))
        ((shr) (encode-shift! s 5 (car args) (cadr args)))
        ((sar) (encode-shift! s 7 (car args) (cadr args)))
        ((mul) (op-rm! s '() #t '(#xf7) 4 (car args) 0 0))
        ((div) (op-rm! s '() #t '(#xf7) 6 (car args) 0 0))
        ((idiv) (op-rm! s '() #t '(#xf7) 7 (car args) 0 0))
        ((cqo) (bytes! s #x48 #x99))
        ((push pop)
         (let ((x (car args)))
           (cond
            ((reg? x)
             (let ((r (reg-num x)))
               (rex! (section-state-buffer s) #f 0 0 r)
               (bytes! s (+ (if (eq? op 'push) #x50 #x58) (logand r 7)))))
            ((and (eq? op 'push) (mem? x)) (op-rm! s '() #f '(#xff) 6 x 0 0))
            ((and (eq? op 'push) (integer? x) (s8? x)) (bytes! s #x6a (logand x 255)))
            ((and (eq? op 'push) (integer? x) (s32? x))
             (bytes! s #x68)
             (buffer-le! (section-state-buffer s) x 4))
            (else (error 'encode-asm "unsupported instruction" instr)))))
        ((call jmp) (encode-transfer! s op (car args) local?))
        ((je jne jl jle jg jge jb jbe ja jae)
         (unless (local? (car args))
           (error 'encode-asm "conditional jump to a symbol of another section" instr))
         (add-item! s 'jump (car args) (cdr (assq op condition-codes))))
        ((leave) (bytes! s #xc9))
        ((ret) (bytes! s #xc3))
        ((syscall) (bytes! s #x0f #x05))
        ((ud2) (bytes! s #x0f #x0b))
        ((rep)
         (case (car args)
           ((movsq) (bytes! s #xf3 #x48 #xa5))
           ((stosq) (bytes! s #xf3 #x48 #xab))
           (else (error 'encode-asm "unsupported instruction" instr))))
        (else
         (error 'encode-asm "unsupported instruction" instr)))))


  ;; Each x an integer, a symbol's address, or (+ symbol n) that address plus n
  (define (quad! s xs)
    (for-each (lambda (x)
                (cond
                 ((string? x)
                  (add-fixup! s 0 'abs64 x 0)
                  (buffer-le! (section-state-buffer s) 0 8))
                 ((pair? x)
                  (add-fixup! s 0 'abs64 (cadr x) (caddr x))
                  (buffer-le! (section-state-buffer s) 0 8))
                 (else
                  (buffer-le! (section-state-buffer s) x 8))))
              xs))


  (define (asciz! s str)
    (let ((b (section-state-buffer s)))
      (buffer-bytevector! b (string->utf8 str))
      (buffer-u8! b 0)))


  ;; GNU as's NOPs for padding code, by length; a longer gap is 11-byte ones, then the rest
  (define nops
    '#((#x90)
       (#x66 #x90)
       (#x0f #x1f #x00)
       (#x0f #x1f #x40 #x00)
       (#x0f #x1f #x44 #x00 #x00)
       (#x66 #x0f #x1f #x44 #x00 #x00)
       (#x0f #x1f #x80 #x00 #x00 #x00 #x00)
       (#x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00)
       (#x66 #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00)
       (#x66 #x2e #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00)
       (#x66 #x66 #x2e #x0f #x1f #x84 #x00 #x00 #x00 #x00 #x00)))


  (define (padding! b n text?)
    (cond
     ((= n 0))
     ((not text?) (buffer-zeros! b n))
     ((<= n 11) (for-each (lambda (x) (buffer-u8! b x)) (vector-ref nops (- n 1))))
     (else
      (for-each (lambda (x) (buffer-u8! b x)) (vector-ref nops 10))
      (padding! b (- n 11) text?))))


  ;; The size of the item at offset
  (define (item-size item offset)
    (case (item-kind item)
      ((label) 0)
      ((jump) (cond
               ((not (item-long? item)) 2)
               ((item-cc item) 6)
               (else 5)))
      (else (modulo (- offset) (item-arg item)))))


  ;; Grows short jumps until they all fit, then gives the offset of each item, a vector.
  ;; As in GNU as, a pass sees the labels it has passed at their new offsets.
  ;; A label ahead is assumed to have moved as much as the jump, unless an
  ;; alignment between them may absorb it: then it is at its old offset.
  ;; Regions are separated by alignments.
  (define (relax! items)
    (let* ((n (vector-length items))
           (old (make-vector n 0))
           (new (make-vector n 0))
           (regions (make-vector n 0)))
      (let pass ()
        (let walk ((i 0) (delta 0) (region 0))
          (when (< i n)
            (let* ((item (vector-ref items i))
                   (offset (+ (item-pos item) delta)))
              (vector-set! old i offset)
              (vector-set! regions i region)
              (walk (+ i 1) (+ delta (item-size item offset))
                    (if (eq? (item-kind item) 'align) (+ region 1) region)))))
        (let walk ((i 0) (delta 0) (changed #f))
          (if (< i n)
              (let* ((item (vector-ref items i))
                     (offset (+ (item-pos item) delta)))
                (vector-set! new i offset)
                (when (and (eq? (item-kind item) 'jump) (not (item-long? item)))
                  (let* ((j (item-target item))
                         ;; #f: a label past an alignment now behind, left short until the next pass
                         (target (cond
                                  ((< j i) (vector-ref new j))
                                  ((= (vector-ref regions j) (vector-ref regions i))
                                   (+ (vector-ref old j) (- offset (vector-ref old i))))
                                  ((< (vector-ref old j) offset) #f)
                                  (else (vector-ref old j)))))
                    (unless (or (not target) (s8? (- target offset 2)))
                      (item-long?-set! item #t)
                      (set! changed #t))))
                (walk (+ i 1) (+ delta (item-size item offset)) changed))
              (when changed
                (pass)))))
      old))


  ;; The bytes of the section, with its items at their offsets
  (define (section-bytes! s items offsets text?)
    (let ((fixed (buffer->bytevector (section-state-buffer s)))
          (b (new-byte-buffer))
          (n (vector-length items)))
      (let loop ((i 0) (pos 0))
        (if (= i n)
            (buffer-copy! b fixed pos (- (bytevector-length fixed) pos))
            (let* ((item (vector-ref items i))
                   (offset (vector-ref offsets i)))
              (buffer-copy! b fixed pos (- (item-pos item) pos))
              (case (item-kind item)
                ((jump)
                 (let ((disp (- (vector-ref offsets (item-target item))
                                (+ offset (item-size item offset))))
                       (cc (item-cc item)))
                   (cond
                    ((not (item-long? item))
                     (buffer-u8! b (if cc (+ #x70 cc) #xeb))
                     (buffer-le! b disp 1))
                    (cc
                     (buffer-u8! b #x0f)
                     (buffer-u8! b (+ #x80 cc))
                     (buffer-le! b disp 4))
                    (else
                     (buffer-u8! b #xe9)
                     (buffer-le! b disp 4)))))
                ((align)
                 (padding! b (item-size item offset) text?)))
              (loop (+ i 1) (item-pos item)))))
      (buffer->bytevector b)))


  ;; Moves the fixups, oldest first, from their position in the buffer to their offset in the
  ;; section: past the items at or before it
  (define (place-fixups! fixups items offsets)
    (let ((n (vector-length items)))
      (let loop ((fixups fixups) (i 0) (delta 0))
        (when (pair? fixups)
          (let ((f (car fixups)))
            (if (and (< i n) (<= (item-pos (vector-ref items i)) (fixup-offset f)))
                (let ((item (vector-ref items i)))
                  (loop fixups (+ i 1)
                        (+ delta (item-size item (vector-ref offsets i)))))
                (begin
                  (fixup-offset-set! f (+ (fixup-offset f) delta))
                  (loop (cdr fixups) i delta))))))))


  ;; The section of each label, and the global names in order
  (define (scan-labels asm)
    (let ((sections (make-hashtable string-hash string=?))
          (current #f))
      (let loop ((asm asm) (globals '()))
        (if (null? asm)
            (values sections (reverse globals))
            (let ((x (car asm)))
              (case (car x)
                ((text) (set! current ".text"))
                ((section) (set! current (cadr x)))
                ((label)
                 (when (hashtable-contains? sections (cadr x))
                   (error 'encode-asm "duplicate label" (cadr x)))
                 (hashtable-set! sections (cadr x) current)))
              (loop (cdr asm) (if (eq? (car x) 'globl) (cons (cadr x) globals) globals)))))))


  ;; The asm s-expressions asm: a unit
  (define (encode-asm asm)
    (let-values (((label-sections globals) (scan-labels asm)))
      (let ((states '())
            (current #f))
        (define (enter! name)
          (set! current
                (or (find (lambda (s) (string=? (section-state-name s) name)) states)
                    (let ((s (make-section-state name (length states) (new-byte-buffer) '() '() 1)))
                      (set! states (cons s states))
                      s))))
        (define (state)
          (or current (error 'encode-asm "code outside of a section")))
        (for-each
         (lambda (x)
           (case (car x)
             ((intel_syntax globl) #f)
             ((text) (enter! ".text"))
             ((section) (enter! (cadr x)))
             ((label) (add-item! (state) 'label (cadr x) #f))
             ((balign)
              (let ((s (state)))
                (add-item! s 'align (cadr x) #f)
                (section-state-align-set! s (max (cadr x) (section-state-align s)))))
             ((quad) (quad! (state) (cdr x)))
             ((asciz) (asciz! (state) (cadr x)))
             ((incbin) (buffer-bytevector! (section-state-buffer (state)) (caddr x)))
             (else
              (let ((s (state)))
                (encode! s x (lambda (name)
                               (and (string? name)
                                    (equal? (hashtable-ref label-sections name #f)
                                            (section-state-name s)))))))))
         asm)
        (let loop ((states (reverse states))
                   (sections '())
                   (labels '())
                   (fixups '()))
          (if (null? states)
              (make-unit (reverse sections) (reverse labels) globals (reverse fixups))
              (let* ((s (car states))
                     (items (list->vector (reverse (section-state-items s))))
                     (indices (make-hashtable string-hash string=?)))
                (do ((i 0 (+ i 1)))
                    ((= i (vector-length items)))
                  (when (eq? (item-kind (vector-ref items i)) 'label)
                    (hashtable-set! indices (item-arg (vector-ref items i)) i)))
                (do ((i 0 (+ i 1)))
                    ((= i (vector-length items)))
                  (let ((item (vector-ref items i)))
                    (when (eq? (item-kind item) 'jump)
                      (item-target-set! item (hashtable-ref indices (item-arg item) #f)))))
                (let ((offsets (relax! items))
                      (own (reverse (section-state-fixups s))))
                  (place-fixups! own items offsets)
                  (loop (cdr states)
                        (cons (make-section (section-state-name s) (section-state-align s)
                                            (section-bytes! s items offsets
                                                            (string=? (section-state-name s) ".text")))
                              sections)
                        (do ((i 0 (+ i 1))
                             (labels labels
                                     (let ((item (vector-ref items i)))
                                       (if (eq? (item-kind item) 'label)
                                           (cons (make-unit-label (item-arg item) (section-state-index s)
                                                                  (vector-ref offsets i))
                                                 labels)
                                           labels))))
                            ((= i (vector-length items)) labels))
                        (append (reverse own) fixups))))))))))
