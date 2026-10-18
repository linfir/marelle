;; Variables and liveness on the lowered IR of one function, shared by
;; copy-prop.sls and reg-alloc.sls.
;;
;; Variables
;; ---------
;; (tmp n) and (raw n), as indexes: (tmp n) is 2n, (raw n) 2n + 1. A set of
;; variables is an integer, one bit per index.
;;
;; Liveness
;; --------
;; A variable is live at a point if its current value may still be read
;; later. Named lets jump backward, so this needs a real analysis, not just
;; the first and last appearance: a tmp read at the top of a loop stays live
;; until the backward jump, past its last appearance.
;;
;; Each block is summarized once: gen, the variables it reads before
;; defining them, kill, the ones it defines, and for each jump the ones defined
;; before it (a jump may sit in the middle of a block). Then the set live at
;; the start of each block is live-in = gen + (live-at-end - kill) + for each
;; jump (live-in of its target - defined before it), with the blocks walked
;; last first, repeating until nothing changes.

(library (flow)
  (export jumps-ops no-dst? var? var-index index-var instr-defs instr-uses
          indexes->set set-minus set-members falls-through? block-starts
          block-liveness live-at-end)
  (import (chezscheme))


  (define jumps-ops '(je jne jl jle jg jge jb jbe ja jae jmp))


  ;; Instructions without a destination
  (define (no-dst? instr)
    (memq (car instr) `(cmp store store8 store32 copy fill ret tailcall selfcall unreachable ,@jumps-ops)))


  (define (var? x)
    (and (pair? x) (memq (car x) '(tmp raw)) #t))


  (define (var-index x)
    (+ (* 2 (cadr x)) (if (eq? (car x) 'raw) 1 0)))


  (define (index-var i)
    (list (if (odd? i) 'raw 'tmp) (quotient i 2)))


  ;; The indexes of the variables in x
  (define (var-indexes x)
    (cond
     ((var? x) (list (var-index x)))
     ((pair? x) (append (var-indexes (car x)) (var-indexes (cdr x))))
     (else '())))


  (define (instr-defs instr)
    (if (no-dst? instr) '() (var-indexes (list (cadr instr)))))


  (define (instr-uses instr)
    (var-indexes (if (no-dst? instr) (cdr instr) (cddr instr))))


  ;; The set of the indexes is, which are few
  (define (indexes->set is)
    (fold-left (lambda (set i) (logor set (ash 1 i))) 0 is))


  (define (set-minus a b)
    (logand a (lognot b)))


  ;; The indexes of a set, jumping from one set bit to the next
  (define (set-members s)
    (let loop ((s s) (acc '()))
      (if (zero? s)
          (reverse acc)
          (let ((i (bitwise-first-bit-set s)))
            (loop (logxor s (ash 1 i)) (cons i acc))))))


  (define (falls-through? block)
    (let ((instrs (cddr block)))
      (not (and (pair? instrs)
                (memq (caar (last-pair instrs)) '(jmp ret tailcall selfcall unreachable))))))


  ;; What liveness needs of a block, computed once: gen, the variables it
  ;; reads before defining them; kill, the ones it defines; jumps, for each
  ;; jump, its target label and the variables defined before it
  (define-record-type summary
    (fields gen kill jumps))


  (define (summarize block)
    (let loop ((instrs (cddr block))
               (gen '())
               (defined '())
               (jumps '()))
      (if (null? instrs)
          (make-summary (indexes->set gen) (indexes->set defined) (reverse jumps))
          (let* ((instr (car instrs))
                 (gen (append (filter (lambda (i) (not (memv i defined))) (instr-uses instr)) gen))
                 (jumps (if (memq (car instr) jumps-ops)
                            (cons (cons (cadr instr) (indexes->set defined)) jumps)
                            jumps)))
            (loop (cdr instrs) gen (append (instr-defs instr) defined) jumps)))))


  ;; The set live at the end of block, falling through to next
  (define (live-at-end block next ins)
    (if (and next (falls-through? block)) (vector-ref ins (cadr next)) 0))


  ;; The position of the first instruction of each block
  (define (block-starts blocks)
    (let loop ((blocks blocks) (start 0) (acc '()))
      (if (null? blocks)
          (reverse acc)
          (loop (cdr blocks) (+ start (length (cddr (car blocks)))) (cons start acc)))))


  ;; The set live at the start of each block, by label (see "Liveness" above):
  ;; live-in = gen + (live-at-end - kill) + for each jump (live-in of its target
  ;; - the variables defined before it). The blocks are walked last first,
  ;; since liveness flows backward, until no set changes: code without loops
  ;; needs one walk, and one more to see that nothing changed.
  (define (block-liveness blocks)
    (let ((ins (make-vector (+ 1 (apply max (map cadr blocks))) 0))
          (walks (reverse (map list blocks (append (cdr blocks) '(#f)) (map summarize blocks)))))
      (let loop ()
        (let ((before (vector-copy ins)))
          (for-each (lambda (w)
                      (apply (lambda (b next sum)
                               (vector-set! ins (cadr b)
                                            (fold-left (lambda (live j)
                                                         (logor live (set-minus (vector-ref ins (car j)) (cdr j))))
                                                       (logor (summary-gen sum)
                                                              (set-minus (live-at-end b next ins) (summary-kill sum)))
                                                       (summary-jumps sum))))
                             w))
                    walks)
          (if (equal? before ins) ins (loop)))))))
