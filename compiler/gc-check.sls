(library (gc-check)
  (export check-gc-safety raws-in)
  (import (chezscheme)
          (utils)
          (flow))


  ;; Sets of raws are integers: bit n stands for (raw n)
  (define (raws-in x)
    (cond
     ((not (pair? x)) 0)
     ((eq? (car x) 'raw) (ash 1 (cadr x)))
     (else (logor (raws-in (car x)) (raws-in (cdr x))))))


  ;; The variables live before instr, given those live after it
  (define (live-before instr live)
    (logor (set-minus live (indexes->set (instr-defs instr)))
           (indexes->set (instr-uses instr))))


  ;; Raw slots hold untagged words, so none may be live across a safepoint
  (define (check-gc-safety name body)
    (let ((ins (block-liveness body)))
      ;; Walks block last first, with the variables live after each instruction
      (define (check-block block next)
        (fold-left (lambda (live instr)
                     (let ((live (if (memq (car instr) jumps-ops)
                                     (logor live (vector-ref ins (cadr instr)))
                                     live)))
                       (when (memq (car instr) '(call tailcall alloc allocframe))
                         (let* ((across (set-minus live (indexes->set (instr-defs instr))))
                                (raw (find odd? (set-members across))))
                           (when raw
                             (compile-error 'check-gc-safety "raw live across a safepoint"
                                    name (index-var raw) instr))))
                       (live-before instr live)))
                   (live-at-end block next ins)
                   (reverse (cddr block))))

      (for-each check-block body (append (cdr body) '(#f))))))
