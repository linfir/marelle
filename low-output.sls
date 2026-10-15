(library (low-output)
  (export immediate lower-block print-entry emit-globals)
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


  (define (print-entry x)
    (if (memq (car x) '(function native))
        (begin
          (printf "(~s ~s ~s" (car x) (cadr x) (caddr x))
          (for-each (lambda (b)
                      (printf "\n  (block ~s" (cadr b))
                      (for-each (lambda (i) (printf "\n    ~s" i)) (cddr b))
                      (display ")"))
                    (cdddr x))
          (display ")\n\n"))
        (printf "~s\n\n" x)))


  ;; The operand for x, or #f when x is not an immediate
  (define (immediate x)
    (define (tag t x)
      `(as-val (quad ,(+ t (* x 8)))))
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
     (else #f)))


  ;; The global cells, initialized to undef
  (define (emit-globals n)
    (print-entry '(export "scheme_globals" "scheme_nglobals"))
    (print-entry `(data "scheme_globals" ,n ,(cadr (lower-operand (immediate 'undef)))))
    (print-entry `(rodata "scheme_nglobals" (quad ,n)))))
