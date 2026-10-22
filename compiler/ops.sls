;; The low operations unsafe defines call, (low name arg ...) after resolution: for each, the
;; kinds of its operands and result, its effect and the block IR instruction it lowers to

(library (ops)
  (export low-ops low-op op-operands op-result op-effect op-instr op-test? low-test)
  (import (chezscheme)
          (utils))


  ;; operands  the kinds of the operands: raw, val, any (a val or a raw), literal (checked by
  ;;           compile-low-op), or a list ending in ... for any number of the kind before it
  ;; result    raw, val, void or never
  ;; effect    pure, reads (memory), writes (memory or the outside world), allocates (a
  ;;           safepoint) or never (does not return)
  ;; instr     the IR instruction, #f for an operand (a constant, a cast, self); for a test,
  ;;           (jump-if-true jump-if-false) after a cmp, its value a boolean
  (define-record-type op
    (fields name operands result effect instr))


  (define (binary name instr)
    (make-op name '(raw raw) 'raw 'pure instr))


  (define (test name jump-if-true jump-if-false)
    (make-op name '(raw raw) 'val 'pure (list jump-if-true jump-if-false)))


  (define table
    (list (binary 'bit-and 'and)
          (binary 'bit-or 'or)
          (binary 'bit-xor 'xor)
          (binary 'raw+ 'add)
          (binary 'raw- 'sub)
          (binary 'raw* 'mul)
          ;; Signed and truncating; a zero divisor traps
          (binary 'raw/ 'div)
          (binary 'raw% 'rem)
          ;; The high 64 bits of the unsigned product
          (binary 'raw*hu 'mulhu)
          (binary 'raw<< 'sal)
          (binary 'raw>> 'sar)
          (binary 'raw>>u 'shr)
          (test 'raw= 'je 'jne)
          (test 'raw< 'jl 'jge)
          (test 'raw<= 'jle 'jg)
          (test 'raw> 'jg 'jle)
          (test 'raw>= 'jge 'jl)
          (test 'raw<u 'jb 'jae)
          (test 'raw<=u 'jbe 'ja)
          (test 'raw>u 'ja 'jbe)
          (test 'raw>=u 'jae 'jb)
          ;; (raw n): the word n
          (make-op 'raw '(literal) 'raw 'pure #f)
          ;; Casts only wrap the operand, except that as-val copies a raw slot into a tmp,
          ;; since the GC scans only tmps
          (make-op 'as-raw '(val) 'raw 'pure #f)
          (make-op 'as-val '(raw) 'val 'pure #f)
          ;; (syscall n arg ...): a Linux system call, n a literal, at most 6 arguments, a
          ;; negative errno on failure; not a safepoint
          (make-op 'syscall '(literal raw ...) 'raw 'writes 'syscall)
          ;; (load address i): the word at word index i of address
          (make-op 'load '(raw raw) 'raw 'reads 'load)
          ;; (store address i x)
          (make-op 'store '(raw raw any) 'void 'writes 'store)
          ;; At byte index i, zero-extended
          (make-op 'load8 '(raw raw) 'raw 'reads 'load8)
          (make-op 'store8 '(raw raw raw) 'void 'writes 'store8)
          (make-op 'load32 '(raw raw) 'raw 'reads 'load32)
          (make-op 'store32 '(raw raw raw) 'void 'writes 'store32)
          ;; (copy-words dst src n), forward: dst may overlap src from below
          (make-op 'copy-words '(raw raw raw) 'void 'writes 'copy)
          ;; (fill-words address n x)
          (make-op 'fill-words '(raw raw raw) 'void 'writes 'fill)
          ;; (raw/wu hi lo d): the unsigned quotient of hi * 2^64 + lo by d, for hi < d
          (make-op 'raw/wu '(raw raw raw) 'raw 'pure 'divwu)
          ;; (alloc n): the raw address of n new zeroed words
          (make-op 'alloc '(raw) 'raw 'allocates 'alloc)
          ;; (alloc-frame n proc): a new frame of n zeroed words, its header and self set; proc
          ;; stays a root while it is allocated
          (make-op 'alloc-frame '(raw val) 'val 'allocates 'allocframe)
          ;; (tail-call-frame f): enters the filled frame f, as a tail call
          (make-op 'tail-call-frame '(val) 'never 'never 'tailcall)
          (make-op 'self '() 'val 'pure #f)
          ;; (address "name"): the address of the symbol name
          (make-op 'address '(literal) 'raw 'pure #f)))


  (define low-ops (map op-name table))


  (define (low-op name)
    (or (find (lambda (op) (eq? (op-name op) name)) table)
        (compile-error 'low-op "unknown low operation" name)))


  (define (op-test? op)
    (pair? (op-instr op)))


  ;; The jumps of the test name, (jump-if-true jump-if-false), or #f
  (define (low-test name)
    (let ((op (find (lambda (op) (eq? (op-name op) name)) table)))
      (and op (op-test? op) (op-instr op)))))
