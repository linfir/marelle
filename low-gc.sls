(library (low-gc)
  (export check-gc-safety raws-in)
  (import (chezscheme))


  (define jumps '(je jne jl jle jg jge jb jbe ja jae jmp))


  ;; Sets of raws are integers: bit n stands for (raw n)
  (define (raws-in x)
    (cond
     ((not (pair? x)) 0)
     ((eq? (car x) 'raw) (ash 1 (cadr x)))
     (else (logor (raws-in (car x)) (raws-in (cdr x))))))


  (define (set-minus a b)
    (logand a (lognot b)))


  ;; Instructions without a destination
  (define (no-dst? instr)
    (memq (car instr) `(cmp store store8 copy fill ret tailcall unreachable ,@jumps)))


  (define (instr-defs instr)
    (if (no-dst? instr) 0 (raws-in (list (cadr instr)))))


  (define (instr-uses instr)
    (raws-in (if (no-dst? instr) (cdr instr) (cddr instr))))


  ;; The raws live before instr, given those live after it
  (define (live-before instr live)
    (logor (set-minus live (instr-defs instr))
           (instr-uses instr)))


  (define (falls-through? block)
    (let ((instrs (cddr block)))
      (not (and (pair? instrs)
                (memq (caar (last-pair instrs)) '(jmp ret tailcall unreachable))))))


  ;; Raw slots hold untagged words, so none may be live across a safepoint
  (define (check-gc-safety name body)
    ;; Maps each block label to the raws live at its start
    (let ((ins (make-vector (+ 1 (apply max (map cadr body))) 0)))
      (define (live-in label)
        (vector-ref ins label))

      ;; The raws live at the end of block, before next
      (define (live-end block next)
        (if (and next (falls-through? block)) (live-in (cadr next)) 0))

      ;; Calls fn on each instruction of block, last first, with the raws live after it;
      ;; returns the raws live at the start of block
      (define (walk-block block next fn)
        (fold-left (lambda (live instr)
                     (let ((live (if (memq (car instr) jumps)
                                     (logor live (live-in (cadr instr)))
                                     live)))
                       (fn instr live)
                       (live-before instr live)))
                   (live-end block next)
                   (reverse (cddr block))))

      (define (walk-all fn)
        (for-each (lambda (b next)
                    (vector-set! ins (cadr b) (walk-block b next fn)))
                  body (append (cdr body) '(#f))))

      (let loop ()
        (let ((before (vector-copy ins)))
          (walk-all (lambda (instr live) #f))
          (unless (equal? before ins)
            (loop))))

      (walk-all (lambda (instr live)
                  (when (memq (car instr) '(call tailcall alloc))
                    (let ((across (set-minus live (instr-defs instr))))
                      (unless (zero? across)
                        (error 'check-gc-safety "raw live across a safepoint"
                               name `(raw ,(bitwise-first-bit-set across)) instr)))))))))
