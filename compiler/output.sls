(library (output)
  (export immediate lower-block global-entries)
  (import (chezscheme))


  (define (lower-operand x)
    (if (pair? x)
        (case (car x)
          ((val) `(tmp ,(cadr x)))
          ((as-raw as-val) (lower-operand (cadr x)))
          (else x))
        x))


  ;; (block label instr ...)
  (define (lower-block block)
    (define (lower-instr instr)
      (cons (car instr) (map lower-operand (cdr instr))))
    `(block ,(cadr block) ,@(map lower-instr (cddr block))))


  ;; The operand for x, or #f when x is not an immediate
  (define (immediate x)
    (let ((word (immediate-word x)))
      (and word `(as-val (quad ,word)))))


  (define (immediate-word x)
    (define (tag t x)
      (+ t (* x 8)))
    (cond
     ((and (integer? x) (exact? x)
           (<= (- (expt 2 60)) x (- (expt 2 60) 1)))
      (tag 1 x))
     ((boolean? x)
      (tag 2 (if x 1 0)))
     ((equal? x ''())
      (tag 2 2))
     ((eq? x 'undef)
      (tag 2 3))
     ((eq? x 'unbound)
      (tag 2 5))
     ((char? x)
      (tag 2 (+ 4 (* 32 (char->integer x)))))
     (else #f)))


  ;; The global cells, initialized to undef, each set by main before it is read
  (define (global-entries n)
    (list `(data "scheme_globals" ,n ,(immediate-word 'undef)))))
