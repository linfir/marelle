;; Copy propagation on the lowered IR of one function.
;;
;; The compiler copies the variables a set! may change: binding an assigned
;; local copies its value, and each read copies it again, so a later set!
;; does not change an operand already computed. Most of these copies are not
;; needed: neither side is written while the copy is read.
;;
;; After (mov d s), a read of d may read s instead if neither d nor s is
;; written on any path from the copy to the read: the copy is available
;; there. Once nothing reads d, the copy is dead and removed.
;;
;; Available copies
;; ----------------
;; A forward analysis, the mirror of liveness: a copy is available at the
;; start of a block if it is available at the end of every predecessor, a
;; jump to the block or the block falling through to it; the first block has
;; none, nor a block without predecessors (unreachable). Each copy is a bit,
;; its number in the function, so a set of copies is an integer; writing a
;; variable kills the copies from and to it. The sets start full and the
;; blocks are walked first to last until nothing changes: a copy stays
;; available at the top of a loop only if the back edge keeps it.
;;
;; The variables here are the tmps, the raws, the args (a set! of an
;; assigned parameter writes its arg) and self. A selfcall writes self and
;; the args, but jumps to the first block, which has no copies.
;;
;; Rewriting
;; ---------
;; Each read of d with an available copy (mov d s) becomes s, repeated while
;; s has one too (a chain of copies, all available, all equal). At most one
;; copy to d is available at a point: a copy to d kills the others. The copy
;; facts are about the original copies, whose sources may be rewritten too:
;; s keeps its value while the copy is available, so the facts stay true.
;;
;; Kinds
;; -----
;; The collector scans val slots, never raw ones, so no raw may be live
;; across a safepoint. In a Scheme function, a copy from a raw to a val (the
;; tmp an as-val gives) is not propagated: reading the raw instead could keep
;; it live across a safepoint. A copy from a val to a raw is: no safepoint
;; separates the copy from the reads of the raw. Noalloc functions have no
;; safepoints. A constant (a quad, a symbol's address) never moves, so it
;; replaces any read.
;;
;; Dead copies
;; -----------
;; Then, with liveness (flow.sls), each copy to a tmp or a raw not live after
;; it is removed, and each copy of a variable to itself. A chain of dead
;; copies in a block goes in one walk, the block walked last first; one
;; across blocks may leave a copy, which would need another liveness pass
;; (as long as the rest of the pass on a large function).
;;
;; Coalescing
;; ----------
;; The copies left mostly merge two paths: the two branches of a value if
;; copy their values to one tmp, a named let copies the new values of its
;; variables to them before jumping back. Both sides of such a move (mov d s),
;; two tmps or two raws, become one variable unless they interfere: one is
;; live after a def of the other, but s after the move itself, where both
;; hold the same value. The move is then a copy of a variable to itself, and
;; goes. The same walk records, for each variable of a move, the variables
;; live after its defs; merged variables merge these sets, and the moves are
;; tried in block order. Reg-alloc could not do it: a variable's interval has
;; no holes, so the source of the second branch's copy lies inside the
;; interval of the destination.

(library (copy-prop)
  (export propagate-copies)
  (import (chezscheme)
          (flow))


  (define (operand-var? x)
    (and (pair? x) (memq (car x) '(tmp raw arg self)) #t))


  (define (instr-dst instr)
    (and (not (no-dst? instr)) (operand-var? (cadr instr)) (cadr instr)))


  ;; A copy this pass may propagate: a mov to a variable from another one or a constant
  (define (copy? instr noalloc?)
    (and (eq? (car instr) 'mov)
         (let ((dst (cadr instr))
               (src (caddr instr)))
           (and (operand-var? dst)
                (not (equal? dst src))
                (or noalloc?
                    (not (and (memq (car dst) '(tmp arg)) (pair? src) (eq? (car src) 'raw))))))))


  ;; The copies of a function: a vector of the copy instructions, by number; a hashtable
  ;; from the position of each to its number; for each variable, the set of the copies
  ;; from or to it (kills), and the numbers of the copies to it (by-dst)
  (define-record-type copies
    (fields instrs numbers kills by-dst))


  (define (find-copies blocks starts noalloc?)
    (let ((numbers (make-eqv-hashtable))
          (kills (make-hashtable equal-hash equal?))
          (by-dst (make-hashtable equal-hash equal?))
          (instrs '())
          (n 0))
      (for-each (lambda (b start)
                  (for-each-instr
                   (lambda (position instr)
                     (when (copy? instr noalloc?)
                       (let ((dst (cadr instr))
                             (src (caddr instr)))
                         (hashtable-set! numbers position n)
                         (hashtable-update! kills dst (lambda (k) (logor k (ash 1 n))) 0)
                         (when (operand-var? src)
                           (hashtable-update! kills src (lambda (k) (logor k (ash 1 n))) 0))
                         (hashtable-update! by-dst dst (lambda (l) (cons n l)) '())
                         (set! instrs (cons instr instrs))
                         (set! n (+ n 1)))))
                   start (cddr b)))
                blocks starts)
      (make-copies (list->vector (reverse instrs)) numbers kills by-dst)))


  (define (for-each-instr f start instrs)
    (let loop ((position start) (instrs instrs))
      (unless (null? instrs)
        (f position (car instrs))
        (loop (+ position 1) (cdr instrs)))))


  ;; The copies available after instr, at position, given those available before it
  (define (transfer copies position instr available)
    (let* ((dst (instr-dst instr))
           (killed (if dst (hashtable-ref (copies-kills copies) dst 0) 0))
           (available (if (zero? killed) available (set-minus available killed)))
           (n (hashtable-ref (copies-numbers copies) position #f)))
      (if n (logor available (ash 1 n)) available)))


  ;; Calls (f label) for the target of each edge, a jump or a fall-through, in block order
  (define (for-each-edge f blocks)
    (for-each (lambda (b next)
                (for-each (lambda (instr)
                            (when (memq (car instr) jumps-ops)
                              (f (cadr instr))))
                          (cddr b))
                (when (and next (falls-through? b))
                  (f (cadr next))))
              blocks (append (cdr blocks) '(#f))))


  ;; The copies available at the start of each block, by label (see "Available copies"
  ;; above). The edges are numbered in block order, each holding the copies available
  ;; when it is taken, and a block gets the intersection of the edges to it. Walking in
  ;; block order updates a forward edge before its target, so only the back edges need
  ;; another walk.
  (define (block-availability blocks starts copies)
    (let* ((size (+ 1 (apply max (map cadr blocks))))
           (all (- (ash 1 (vector-length (copies-instrs copies))) 1))
           (preds (make-vector size '()))
           (ins (make-vector size 0))
           (nedges (let ((k 0))
                     (for-each-edge (lambda (label)
                                      (vector-set! preds label (cons k (vector-ref preds label)))
                                      (set! k (+ k 1)))
                                    blocks)
                     k))
           (edges (make-vector nedges all)))
      (let loop ()
        (let ((changed #f)
              (k 0))
          (define (edge! available)
            (unless (= available (vector-ref edges k))
              (vector-set! edges k available)
              (set! changed #t))
            (set! k (+ k 1)))
          (for-each (lambda (b next start)
                      (let ((in (if (or (eq? b (car blocks)) (null? (vector-ref preds (cadr b))))
                                    0
                                    (fold-left (lambda (acc e) (logand acc (vector-ref edges e)))
                                               all (vector-ref preds (cadr b))))))
                        (vector-set! ins (cadr b) in)
                        (let walk ((position start)
                                   (instrs (cddr b))
                                   (available in))
                          (if (null? instrs)
                              (when (and next (falls-through? b))
                                (edge! available))
                              (let ((instr (car instrs)))
                                (when (memq (car instr) jumps-ops)
                                  (edge! available))
                                (walk (+ position 1) (cdr instrs)
                                      (transfer copies position instr available)))))))
                    blocks (append (cdr blocks) '(#f)) starts)
          (if changed (loop) ins)))))


  ;; x, or the source of the chain of copies available from it
  (define (copy-source copies available x)
    (let ((n (and (operand-var? x)
                  (find (lambda (n) (logbit? n available))
                        (hashtable-ref (copies-by-dst copies) x '())))))
      (if n
          (copy-source copies available (caddr (vector-ref (copies-instrs copies) n)))
          x)))


  (define (rewrite-reads copies available instr)
    (let ((copy-source (lambda (x) (copy-source copies available x))))
      (if (no-dst? instr)
          (cons (car instr) (map copy-source (cdr instr)))
          (cons* (car instr) (cadr instr) (map copy-source (cddr instr))))))


  (define (rewrite-blocks blocks starts copies ins)
    (map (lambda (b start)
           (let walk ((position start)
                      (instrs (cddr b))
                      (available (vector-ref ins (cadr b)))
                      (acc '()))
             (if (null? instrs)
                 `(block ,(cadr b) ,@(reverse acc))
                 (let ((instr (car instrs)))
                   (walk (+ position 1) (cdr instrs)
                         (transfer copies position instr available)
                         (cons (rewrite-reads copies available instr) acc))))))
         blocks starts))


  (define (self-copy? instr)
    (and (eq? (car instr) 'mov) (equal? (cadr instr) (caddr instr))))


  ;; A copy between two variables of the same kind, which coalescing may merge
  (define (move? instr)
    (and (eq? (car instr) 'mov)
         (var? (cadr instr))
         (var? (caddr instr))
         (eq? (caadr instr) (caaddr instr))))


  ;; The blocks without the copies to a tmp or a raw dead after them and the copies of a
  ;; variable to itself, and for each variable of a move, the set live after its defs
  ;; but the source of a move to it
  (define (remove-dead-copies blocks)
    (let ((ins (block-liveness blocks))
          (moved (fold-left (lambda (set instr)
                              (if (move? instr)
                                  (logor set (indexes->set (append (instr-defs instr) (instr-uses instr))))
                                  set))
                            0 (apply append (map cddr blocks))))
          (live-at-defs (make-eqv-hashtable)))
      (define (record-defs! instr live)
        (for-each (lambda (i)
                    (when (logbit? i moved)
                      (let ((source (if (move? instr) (indexes->set (instr-uses instr)) 0)))
                        (hashtable-update! live-at-defs i
                                           (lambda (s) (logor s (set-minus live (logor source (ash 1 i)))))
                                           0))))
                  (instr-defs instr)))
      (values
       (map (lambda (b next)
              (let loop ((instrs (reverse (cddr b)))
                         (live (live-at-end b next ins))
                         (acc '()))
                (if (null? instrs)
                    `(block ,(cadr b) ,@acc)
                    (let* ((instr (car instrs))
                           (live (if (memq (car instr) jumps-ops)
                                     (logor live (vector-ref ins (cadr instr)))
                                     live)))
                      (if (or (self-copy? instr)
                              (and (eq? (car instr) 'mov)
                                   (var? (cadr instr))
                                   (not (logbit? (var-index (cadr instr)) live))))
                          (loop (cdr instrs) live acc)
                          (begin
                            (record-defs! instr live)
                            (loop (cdr instrs)
                                  (logor (set-minus live (indexes->set (instr-defs instr)))
                                         (indexes->set (instr-uses instr)))
                                  (cons instr acc))))))))
            blocks (append (cdr blocks) '(#f)))
       live-at-defs)))


  ;; The blocks with the two sides of each move renamed to one variable when they do
  ;; not interfere, and the moves that leaves from a variable to itself removed
  (define (coalesce blocks live-at-defs)
    (let ((parents (make-eqv-hashtable))
          (members (make-eqv-hashtable))
          (merged #f))
      (define (find i)
        (let ((p (hashtable-ref parents i i)))
          (if (= p i) i (find p))))
      (define (live-at-defs-of i)
        (hashtable-ref live-at-defs i 0))
      (define (members-of i)
        (hashtable-ref members i (ash 1 i)))
      (for-each (lambda (b)
                  (for-each (lambda (instr)
                              (when (move? instr)
                                (let ((a (find (var-index (cadr instr))))
                                      (b (find (var-index (caddr instr)))))
                                  (when (and (not (= a b))
                                             (zero? (logand (live-at-defs-of a) (members-of b)))
                                             (zero? (logand (live-at-defs-of b) (members-of a))))
                                    (let ((root (min a b))
                                          (other (max a b)))
                                      (hashtable-set! parents other root)
                                      (hashtable-set! members root (logor (members-of a) (members-of b)))
                                      (hashtable-set! live-at-defs root
                                                      (logor (live-at-defs-of a) (live-at-defs-of b)))
                                      (set! merged #t))))))
                            (cddr b)))
                blocks)
      (if (not merged)
          blocks
          (let ()
            (define (rename x)
              (cond
               ((var? x) (index-var (find (var-index x))))
               ((pair? x) (map rename x))
               (else x)))
            (map (lambda (b)
                   `(block ,(cadr b) ,@(filter (lambda (instr) (not (self-copy? instr)))
                                               (map rename (cddr b)))))
                 blocks)))))


  ;; The blocks of a function, lowered, with the reads of copied variables rewritten
  ;; and the dead copies removed
  (define (propagate-copies blocks noalloc?)
    (let* ((starts (block-starts blocks))
           (copies (find-copies blocks starts noalloc?))
           (blocks (if (zero? (vector-length (copies-instrs copies)))
                       blocks
                       (rewrite-blocks blocks starts copies
                                       (block-availability blocks starts copies)))))
      (let-values (((blocks live-at-defs) (remove-dead-copies blocks)))
        (coalesce blocks live-at-defs)))))
