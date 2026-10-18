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
;; later; block-liveness (flow.sls) gives the set live at the start of each
;; block.
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
;; An interval ending where another starts, at position p, ends at a read
;; there: a jump defines nothing. asm reads the operands of an instruction
;; before writing its destination, so the variable defined at p may take the
;; register of one read for the last time at p: (add (raw 2) (raw 1) (quad 1))
;; becomes add rsi, 1 when raw 1 dies there. The registers freed at p come
;; first, so they are preferred. Not for frame slots: a save around the call
;; at p writes its slot before the call reads its operands.
;;
;; The scan is linear: one pass, no backtracking. A variable is in one
;; register or one frame slot for its whole life (no interval splitting).
;;
;; Registers and calls
;; -------------------
;; There are two classes of registers. asm and the kernel never write rbx
;; and r12-r15 but at calls. rsi, rdi and r8-r10 are written by asm at the
;; clobbering instructions: syscall (arguments), copy and fill (rep movsq, rep
;; stosq) and selfcall (its operands). An interval containing one of these,
;; past its start, only gets a register of the first class; another prefers
;; one of the second, keeping the first ones for the former. A clobbering
;; instruction at the start is the def: asm writes the destination after
;; clobbering.
;;
;; A call, Scheme (call) or native (stackcall), clobbers all registers: there
;; are no callee-saved registers, so that every live value is in its own
;; frame at a Scheme call, which keeps the GC and continuations simple. An
;; alloc may run the GC, whose only root is the frame: a pointer kept in a
;; register would not be moved.
;;
;; So each register holding a variable live after a call or an alloc is saved
;; to a frame slot just before it and restored just after it (caller-save).
;; The destination of the call is neither saved nor restored. No raw is live
;; across a Scheme call or an alloc (check-gc-safety), but raws may be live
;; across a native call, which is not a safepoint: they are saved to raw
;; slots. Only live values are saved, at each call they cross: cheaper than
;; spilling for values used often between calls, dearer for values crossing
;; many calls but rarely used.
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
  (export allocate-registers)
  (import (chezscheme)
          (utils)
          (flow))


  ;; Never written by asm or the kernel but at calls
  (define kept-registers '(rbx r12 r13 r14 r15))


  ;; Written by asm only at the clobbering instructions and calls
  (define scratch-registers '(rsi rdi r8 r9 r10))


  (define clobbering-ops '(syscall copy fill selfcall))


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


  ;; For each position, the position of the first clobbering instruction after
  ;; it, or the number of instructions
  (define (next-clobbers blocks)
    (let* ((instrs (list->vector (apply append (map cddr blocks))))
           (n (vector-length instrs))
           (next (make-vector n n)))
      (do ((p (- n 2) (- p 1))) ((< p 0) next)
        (vector-set! next p (if (memq (car (vector-ref instrs (+ p 1))) clobbering-ops)
                                (+ p 1)
                                (vector-ref next (+ p 1)))))))


  ;; Insert iv into active, a list of intervals sorted by increasing
  ;; interval-end (ties go after): the last one is the spill candidate in sweep!
  (define (insert-by-end iv active)
    (let-values (((rev-before after)
                  (span-reverse (lambda (a) (<= (interval-end a) (interval-end iv)))
                                active)))
      (append-reverse rev-before (cons iv after))))


  ;; Assign a location from locs, or 'spill, to each interval, sorted by start
  ;; (see "The scan" above). Sets interval-loc, returns nothing useful.
  ;; (rank iv loc) is #f if iv cannot take loc, else a number, lower being
  ;; better.
  ;;
  ;; A location is the IR operand that rewrite puts in place of the variable;
  ;; sweep! never looks inside one. allocate-registers sweeps three times:
  ;;
  ;; - over the variable intervals, with locs = ((reg r) ...), one per
  ;;   register: each gets a (reg r) or 'spill (not a location: none it can
  ;;   take was left, see "Registers and calls" above);
  ;; - over the spilled tmps and the saves around calls, with
  ;;   locs = ((tmp 1) ... (tmp n)), one val frame slot per interval, so
  ;;   nothing is spilled: each 'spill is overwritten with a slot (tmp k);
  ;; - over the spilled raws, the same way with raw frame slots (raw k).
  ;;
  ;; The loop carries active, the intervals holding a location, sorted by
  ;; end (insert-by-end), and free, the locations nobody holds. For each iv:
  ;;
  ;; 1. Intervals of active ending before iv starts, or where it starts if
  ;;    touching?, are dropped, and their locations go back to free, the last
  ;;    freed first.
  ;; 2. If a free location fits iv, iv takes the best one and joins active.
  ;; 3. Otherwise, if the last of active holding a location that fits iv ends
  ;;    after iv, it is the victim: iv takes its location and replaces it in
  ;;    active, the victim is spilled.
  ;; 4. Otherwise iv is spilled, and active is unchanged.
  ;;
  ;; A spilled interval never gets a location back.
  (define (sweep! intervals locs rank touching?)
    (let loop ((ivs intervals)
               (active '())
               (free locs))
      (unless (null? ivs)
        (let ((iv (car ivs)))
          ;; active is sorted by end, so the expired intervals are a prefix
          (let-values (((expired active)
                        (span (lambda (a)
                                (or (< (interval-end a) (interval-start iv))
                                    (and touching? (= (interval-end a) (interval-start iv)))))
                              active)))
            (let* ((free (append-reverse (map interval-loc expired) free))
                   (loc (best-loc iv free rank)))
              (cond
               (loc
                (interval-loc-set! iv loc)
                (loop (cdr ivs) (insert-by-end iv active) (if (eq? loc (car free)) (cdr free) (remq loc free))))
               (else
                (let ((victim (find (lambda (a) (rank iv (interval-loc a))) (reverse active))))
                  (cond
                   ((and victim (> (interval-end victim) (interval-end iv)))
                    (interval-loc-set! iv (interval-loc victim))
                    (interval-loc-set! victim 'spill)
                    (loop (cdr ivs) (insert-by-end iv (remq victim active)) free))
                   (else
                    (interval-loc-set! iv 'spill)
                    (loop (cdr ivs) active free))))))))))))


  ;; The location of free with the lowest rank for iv, the first one of rank 0,
  ;; or #f if none fits
  (define (best-loc iv free rank)
    (let loop ((free free) (best #f) (best-rank #f))
      (if (null? free)
          best
          (let ((r (rank iv (car free))))
            (cond
             ((eqv? r 0) (car free))
             ((and r (or (not best-rank) (< r best-rank))) (loop (cdr free) (car free) r))
             (else (loop (cdr free) best best-rank)))))))


  ;; Ties broken by variable, so the output is deterministic
  (define (sort-by-start intervals)
    (list-sort (lambda (a b)
                 (or (< (interval-start a) (interval-start b))
                     (and (= (interval-start a) (interval-start b))
                          (< (var-index (interval-var a)) (var-index (interval-var b))))))
               intervals))


  (define (register? loc)
    (and (pair? loc) (eq? (car loc) 'reg)))


  ;; For each call or alloc, the registers to save around it, as a hashtable
  ;; from its position to ((reg . save-interval) ...): the variables in a
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
                        (when (memq (car instr) '(call stackcall alloc allocframe))
                          (hashtable-set!
                           saves position
                           (fold-right
                            (lambda (i acc)
                              (let ((var (index-var i)))
                                (if (not (equal? var (cadr instr)))
                                    (cons (cons (hashtable-ref locs i #f)
                                                (make-interval var position position #f))
                                          acc)
                                    acc)))
                            '() live)))
                        (let ((live (filter (lambda (i) (not (memv i (instr-defs instr)))) live)))
                          (loop (cdr instrs)
                                (- position 1)
                                (append (filter (lambda (i) (and (in-register? i) (not (memv i live))))
                                                (instr-uses instr))
                                        live)))))))
                blocks (append (cdr blocks) '(#f)) starts)
      saves))


  ;; The intervals of a kind of slot get the locations (kind 1) ... (kind n)
  (define (allocate-slots! intervals kind)
    (sweep! (sort-by-start intervals)
            (map (lambda (k) (list kind (+ k 1))) (iota (length intervals)))
            (lambda (iv loc) 0)
            #f))


  ;; Replace each variable in x by its location
  (define (rewrite x locs)
    (cond
     ((var? x) (hashtable-ref locs (var-index x) #f))
     ((pair? x) (map (lambda (y) (rewrite y locs)) x))
     (else x)))


  ;; The blocks of a function with each (tmp n) and (raw n) replaced by a
  ;; register (reg r) or a frame slot of its kind, (tmp k) or (raw k), and the
  ;; registers saved around calls and allocs
  (define (allocate-registers blocks)
    (let* ((starts (block-starts blocks))
           (ins (block-liveness blocks))
           (intervals (sort-by-start (var-intervals blocks starts ins)))
           (locs (make-eqv-hashtable))
           (next (next-clobbers blocks)))
      (define (register-rank iv loc)
        (let ((crosses? (<= (vector-ref next (interval-start iv)) (interval-end iv))))
          (if (memq (cadr loc) kept-registers)
              (if crosses? 0 1)
              (and (not crosses?) 0))))
      (define (spilled kind)
        (filter (lambda (iv) (and (eq? (interval-loc iv) 'spill) (eq? (car (interval-var iv)) kind)))
                intervals))
      (define (record-locs!)
        (for-each (lambda (iv) (hashtable-set! locs (var-index (interval-var iv)) (interval-loc iv)))
                  intervals))
      (sweep! intervals
              (map (lambda (r) (list 'reg r)) (append scratch-registers kept-registers))
              register-rank
              #t)
      (record-locs!)
      (let* ((saves (call-saves blocks starts ins locs))
             (save-intervals (apply append (map (lambda (s) (map cdr s))
                                                (vector->list (hashtable-values saves))))))
        (define (saved kind)
          (filter (lambda (iv) (eq? (car (interval-var iv)) kind)) save-intervals))
        (allocate-slots! (append (spilled 'tmp) (saved 'tmp)) 'tmp)
        (allocate-slots! (append (spilled 'raw) (saved 'raw)) 'raw)
        (record-locs!)
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
             blocks starts)))))
