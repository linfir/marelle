;; Linear scan register allocation of the IR tmps and raws of one function.
;;
;; Variables
;; ---------
;; The IR has two kinds of variables: (tmp n), a tagged value the GC may
;; read, and (raw n), a plain word it never reads. Both may get a register;
;; a variable without one gets a frame slot of its own kind, since the GC
;; scans the val slots and skips the raw ones.
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
;; last first, repeating until nothing changes. Sets of variables are
;; integers, one bit per variable (var-index).
;;
;; Intervals
;; ---------
;; The instructions are numbered in block order. A variable's interval goes
;; from the first to the last position where it is defined, read or live
;; after: two variables whose intervals overlap cannot share a register; two
;; whose intervals do not overlap can. One interval per variable, with no
;; holes: a variable dead in the middle of its interval still keeps its
;; register there. Simple, sometimes one register more than needed.
;;
;; Only the extremes matter, so the sets live after each instruction are not
;; kept: the interval covers each def and use, and each backward jump to a
;; block the variable is live into (a value carried around a loop stays live
;; until the jump back). A forward edge needs nothing: the variable is read
;; after it. Every variable is defined before it is read in block order (named
;; let variables get their first value before the loop), so the interval
;; starts at a def.
;;
;; The scan (sweep!)
;; -----------------
;; Think of registers as parking spots and variables as cars, each arriving
;; at the start of its interval and leaving at its end. Cars are handled in
;; order of arrival:
;;
;; 1. When a car arrives, the spots of the cars already gone are freed.
;; 2. If a spot is free, the car takes it.
;; 3. Otherwise one car goes to the overflow lot (it is spilled to the
;;    frame): the one staying the longest, which frees a spot for as long as
;;    possible. If a parked car stays longer than the new one, it is moved
;;    out and the new one takes its spot; otherwise the new one is spilled.
;;
;; active holds the parked cars sorted by leaving time (insert-by-end), so the
;; one staying the longest is the last one.
;;
;; Example with two registers R1 and R2:
;;
;;   position  0 1 2 3 4 5 6 7 8 9
;;   a         |---------------|        a takes R1
;;   b           |---|                  b takes R2
;;   c             |---|                full: a ends after c, a is spilled,
;;                                      c takes R1
;;   d               |-----------|      full: d ends last, d is spilled
;;   e                   |-|            b and c are gone: e takes R2
;;
;; An interval ending at position p is still live for one starting at p
;; (strict < in the expiry test), so (mov (tmp 2) (tmp 1)) never puts both
;; in the same register. Safe, sometimes one register more than needed.
;;
;; The scan is linear: one pass, no backtracking. A variable is in one
;; register or one frame slot for its whole life (no interval splitting).
;;
;; Registers and calls
;; -------------------
;; The registers (rbx, r12-r15) are callee-saved in System V, and asm never
;; uses them as scratch registers, so a native call (ccall) and a system call
;; preserve them: a native saves the ones it uses (see asm.sls). A Scheme call
;; clobbers all of them: Scheme has no callee-saved registers, so that every
;; live value is in its own frame at a call, which keeps the GC and
;; continuations simple. An alloc may run the GC, whose only root is the
;; frame: a pointer kept in a register would not be moved.
;;
;; So in a Scheme function, each register holding a tmp live after a Scheme
;; call or an alloc is saved to a frame slot just before it and restored just
;; after it (caller-save). The destination of the call is neither saved nor
;; restored. No raw is live across them (check-gc-safety), so raws are never
;; saved. Only live values are saved, at each call they cross: cheaper than
;; spilling for values used often between calls, dearer for values crossing
;; many calls but rarely used. A native never calls Scheme or allocates, so
;; it saves nothing.
;;
;; Frame slots
;; -----------
;; Spilled variables and saved registers are the same kind of thing: a value
;; living in a frame slot over an interval, the variable's whole interval for
;; a spill, [p, p] for a save around the call at p. The slots are allocated by
;; a second sweep! over these intervals, one per kind of slot, with as many
;; slots as intervals, so nothing is spilled and slots are shared by
;; intervals that do not overlap. With 100 tmps each live across its own
;; call, one slot is enough.
;;
;; The output keeps the IR's slot names: (tmp k) is now val frame slot k and
;; (raw k) raw frame slot k, both counting from 1, so asm lays the frame out
;; as before; (reg r) is a register.

(library (reg-alloc)
  (export allocate-registers allocatable-registers)
  (import (chezscheme)
          (utils))


  ;; Callee-saved in System V, and never used by asm as scratch registers
  (define allocatable-registers '(rbx r12 r13 r14 r15))


  (define jumps-ops '(je jne jl jle jg jge jb jbe ja jae jmp))


  ;; Instructions without a destination
  (define (no-dst? instr)
    (memq (car instr) `(cmp store store8 copy fill ret tailcall unreachable ,@jumps-ops)))


  (define (var? x)
    (and (pair? x) (memq (car x) '(tmp raw)) #t))


  ;; Variables as indexes: (tmp n) is 2n, (raw n) 2n + 1; a set of
  ;; variables is an integer, one bit per index
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
                (memq (caar (last-pair instrs)) '(jmp ret tailcall unreachable))))))


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
          (if (equal? before ins) ins (loop))))))


  (define-record-type interval
    (fields var (mutable start) (mutable end) (mutable loc)))


  ;; The interval of each variable, as a list (see "Intervals" above): it
  ;; covers its defs and uses, and each backward jump to a block it is live
  ;; into. A forward edge needs nothing: the variable is read after it. An
  ;; interval starts at a def: the saves around calls rely on it, since a
  ;; register is only saved once its variable has written it.
  (define (var-intervals blocks starts ins)
    (let ((table (make-eqv-hashtable))
          (label-starts (map cons (map cadr blocks) starts)))
      (define (extend! indexes position def?)
        (for-each (lambda (i)
                    (let ((iv (hashtable-ref table i #f)))
                      (cond
                       ((not iv)
                        (unless def?
                          (error 'var-intervals "variable read before its first definition" (index-var i)))
                        (hashtable-set! table i (make-interval (index-var i) position position #f)))
                       ((< position (interval-start iv))
                        (interval-start-set! iv position))
                       ((> position (interval-end iv))
                        (interval-end-set! iv position)))))
                  indexes))
      (for-each (lambda (b start)
                  (for-each-idx (lambda (i instr)
                                  (extend! (instr-uses instr) i #f)
                                  (extend! (instr-defs instr) i #t)
                                  (when (and (memq (car instr) jumps-ops)
                                             (<= (cdr (assv (cadr instr) label-starts)) i))
                                    (extend! (set-members (vector-ref ins (cadr instr))) i #f)))
                                start (cddr b)))
                blocks starts)
      (vector->list (hashtable-values table))))


  ;; Insert iv into active, a list of intervals sorted by increasing
  ;; interval-end (ties go after): the last one is the spill candidate in sweep!
  (define (insert-by-end iv active)
    (let-values (((rev-before after)
                  (span-reverse (lambda (a) (<= (interval-end a) (interval-end iv)))
                                active)))
      (append-reverse rev-before (cons iv after))))


  ;; Assign a location from locs, or 'spill, to each interval, sorted by start
  ;; (see "The scan" above). Sets interval-loc, returns nothing useful.
  ;;
  ;; A location is the IR operand that rewrite puts in place of the variable;
  ;; sweep! never looks inside one. allocate-registers sweeps three times:
  ;;
  ;; - over the variable intervals, with locs = ((reg r) ...), one per
  ;;   register: each gets a (reg r) or 'spill (not a location: none was left);
  ;; - over the spilled tmps and the saves around calls, with
  ;;   locs = ((tmp 1) ... (tmp n)), one val frame slot per interval, so
  ;;   nothing is spilled: each 'spill is overwritten with a slot (tmp k);
  ;; - over the spilled raws, the same way with raw frame slots (raw k).
  ;;
  ;; The loop carries active, the intervals holding a location, sorted by
  ;; end (insert-by-end), and free, the locations nobody holds. For each iv:
  ;;
  ;; 1. Intervals of active ending before iv starts are dropped, and their
  ;;    locations go back to free.
  ;; 2. If a location is free, iv takes it and joins active.
  ;; 3. Otherwise, if the last of active ends after iv, it is the victim:
  ;;    iv takes its location and replaces it in active, the victim is
  ;;    spilled.
  ;; 4. Otherwise iv is spilled, and active is unchanged.
  ;;
  ;; A spilled interval never gets a location back.
  (define (sweep! intervals locs)
    (let loop ((ivs intervals)
               (active '())
               (free locs))
      (unless (null? ivs)
        (let ((iv (car ivs)))
          ;; active is sorted by end, so the expired intervals are a prefix
          (let-values (((expired active)
                        (span (lambda (a) (< (interval-end a) (interval-start iv)))
                              active)))
            (let ((free (append (map interval-loc expired) free)))
              (cond
               ((pair? free)
                (interval-loc-set! iv (car free))
                (loop (cdr ivs) (insert-by-end iv active) (cdr free)))
               ((and (pair? active)
                     (> (interval-end (car (last-pair active))) (interval-end iv)))
                (let ((victim (car (last-pair active))))
                  (interval-loc-set! iv (interval-loc victim))
                  (interval-loc-set! victim 'spill)
                  (loop (cdr ivs) (insert-by-end iv (remq victim active)) free)))
               (else
                (interval-loc-set! iv 'spill)
                (loop (cdr ivs) active free)))))))))


  ;; Ties broken by variable, so the output is deterministic
  (define (sort-by-start intervals)
    (list-sort (lambda (a b)
                 (or (< (interval-start a) (interval-start b))
                     (and (= (interval-start a) (interval-start b))
                          (< (var-index (interval-var a)) (var-index (interval-var b))))))
               intervals))


  (define (register? loc)
    (and (pair? loc) (eq? (car loc) 'reg)))


  ;; For each Scheme call or alloc, the registers to save around it, as a
  ;; hashtable from its position to ((reg . save-interval) ...): the tmps in a
  ;; register live after it, but its destination. Two variables live at the
  ;; same point have overlapping intervals, so at most one per register is
  ;; live: the walk keeps those as a short list. A save interval keeps the
  ;; variable it saves, only for sort-by-start.
  (define (call-saves blocks starts ins locs)
    (let* ((saves (make-eqv-hashtable))
           (in-register? (lambda (i) (register? (hashtable-ref locs i #f))))
           (registers (indexes->set (filter in-register? (vector->list (hashtable-keys locs))))))
      (define (live-registers set)
        (set-members (logand set registers)))
      (for-each (lambda (b next start)
                  (let loop ((instrs (reverse (cddr b)))
                             (position (+ start (length (cddr b)) -1))
                             (live (live-registers (live-at-end b next ins))))
                    (unless (null? instrs)
                      (let* ((instr (car instrs))
                             (live (if (memq (car instr) jumps-ops)
                                       (fold-left (lambda (acc i) (if (memv i acc) acc (cons i acc)))
                                                  live (live-registers (vector-ref ins (cadr instr))))
                                       live)))
                        (when (memq (car instr) '(call alloc))
                          (hashtable-set!
                           saves position
                           (fold-right
                            (lambda (i acc)
                              (let ((var (index-var i)))
                                (if (and (eq? (car var) 'tmp) (not (equal? var (cadr instr))))
                                    (cons (cons (hashtable-ref locs i #f)
                                                (make-interval var position position #f))
                                          acc)
                                    acc)))
                            '() live)))
                        (loop (cdr instrs)
                              (- position 1)
                              (append (filter (lambda (i) (and (in-register? i) (not (memv i live))))
                                              (instr-uses instr))
                                      (filter (lambda (i) (not (memv i (instr-defs instr)))) live)))))))
                blocks (append (cdr blocks) '(#f)) starts)
      saves))


  ;; The intervals of a kind of slot get the locations (kind 1) ... (kind n)
  (define (allocate-slots! intervals kind)
    (sweep! (sort-by-start intervals)
            (map (lambda (k) (list kind (+ k 1))) (iota (length intervals)))))


  ;; Replace each variable in x by its location
  (define (rewrite x locs)
    (cond
     ((var? x) (hashtable-ref locs (var-index x) #f))
     ((pair? x) (map (lambda (y) (rewrite y locs)) x))
     (else x)))


  ;; The blocks of a function with each (tmp n) and (raw n) replaced by a
  ;; register (reg r) or a frame slot of its kind, (tmp k) or (raw k), and the
  ;; registers saved around Scheme calls and allocs, unless native?.
  ;; Returns the new blocks and the registers used.
  (define (allocate-registers blocks native?)
    (let* ((starts (block-starts blocks))
           (ins (block-liveness blocks))
           (intervals (sort-by-start (var-intervals blocks starts ins)))
           (locs (make-eqv-hashtable)))
      (define (spilled kind)
        (filter (lambda (iv) (and (eq? (interval-loc iv) 'spill) (eq? (car (interval-var iv)) kind)))
                intervals))
      (define (record-locs!)
        (for-each (lambda (iv) (hashtable-set! locs (var-index (interval-var iv)) (interval-loc iv)))
                  intervals))
      (sweep! intervals (map (lambda (r) (list 'reg r)) allocatable-registers))
      (record-locs!)
      (let ((saves (if native? (make-eqv-hashtable) (call-saves blocks starts ins locs))))
        (allocate-slots! (append (spilled 'tmp)
                                 (apply append (map (lambda (s) (map cdr s))
                                                    (vector->list (hashtable-values saves)))))
                         'tmp)
        (allocate-slots! (spilled 'raw) 'raw)
        (record-locs!)
        (values
         (map (lambda (b start)
                `(block ,(cadr b)
                        ,@(apply append
                                 (map (lambda (instr position)
                                        (let ((save (hashtable-ref saves position '())))
                                          (append (map (lambda (s) `(mov ,(interval-loc (cdr s)) ,(car s))) save)
                                                  (list (rewrite instr locs))
                                                  (map (lambda (s) `(mov ,(car s) ,(interval-loc (cdr s)))) save))))
                                      (cddr b)
                                      (map (lambda (k) (+ start k)) (iota (length (cddr b))))))))
              blocks starts)
         (filter (lambda (r)
                   (exists (lambda (iv) (equal? (interval-loc iv) (list 'reg r))) intervals))
                 allocatable-registers))))))
