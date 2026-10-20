;; Functions: the blocks of each function to its entries, and the helpers on block operands the
;; conversion and the lowering share.
;;
;; There are 4 kinds:
;; - raw    a plain word
;; - val    a tagged value
;; - never  does not return
;; - void   returns nothing

(library (compile)
  (export kind check-kind literal-ref finish-result emit-function!)
  (import (chezscheme)
          (utils)
          (defs)
          (gc-check)
          (output)
          (emit)
          (copy-prop))


  (define (kind x)
    (cond
     ((string? x) 'raw)
     ((memq (car x) '(quad as-raw)) 'raw)
     ((memq (car x) '(raw never void)) (car x))
     (else 'val)))


  ;; k is a kind, or any for a val or a raw
  (define (check-kind x k expr)
    (unless (if (eq? k 'any) (memq (kind x) '(val raw)) (eq? (kind x) k))
      (compile-error 'check-kind (if (eq? k 'any) "expected a val or a raw" (format "expected a ~a" k))
                     expr)))


  ;; The load of the literal cell of the string or symbol x
  (define (literal-ref x)
    (emit-op! 'load 'val (list (literals-symbol) `(quad ,(intern-literal! x)))))


  (define (finish-result def op)
    (let ((result (def-result def)))
      (cond
       ((eq? (kind op) 'never)
        op)
       ((eq? result 'never)
        (emit! '(unreachable))
        '(never))
       ((eq? result 'void)
        '(void))
       (else
        (check-kind op result (def-name def)) op))))


  ;; Emits the function of def, its blocks emitted by (emit), and adds its entries: a noalloc
  ;; def's native function, or a procedure object and its function
  (define (emit-function! def emit)
    (let ((blocks (with-function emit)))
      (check-gc-safety (def-name def) blocks)
      (if (def-noalloc? def)
          (begin
            (check-noalloc (def-name def) blocks)
            (add-entries! `((noalloc ,(def-code def) (arity ,(length (def-params def)))
                                     ,@(lower-function (def-code def) blocks #t)))))
          (add-entries! (procedure-entries def blocks)))))


  ;; A noalloc def may run during a collection, so it never allocates or
  ;; calls Scheme; it has no procedure object, so no self
  (define (check-noalloc name body)
    (define (self? x)
      (and (pair? x)
           (or (equal? x '(self))
               (self? (car x))
               (self? (cdr x)))))

    (for-each (lambda (block)
                (for-each (lambda (instr)
                            (when (or (memq (car instr) '(call tailcall selfcall alloc allocframe))
                                      (self? instr))
                              (compile-error 'check-noalloc "safepoint or self in a noalloc def" name instr)))
                          (cddr block)))
              body))


  ;; A general def is named after its specialization
  (define (procedure-name def)
    (let ((s (symbol->string (def-name def))))
      (if (and (> (string-length s) 9) (string=? (substring s 0 9) "variadic-"))
          (substring s 9 (string-length s))
          s)))


  ;; The def's name, procedure object and function; a closure def's object is the template
  ;; alloc-closure copies, with its ncaptures
  (define (procedure-entries def blocks)
    (let* ((proc (def-proc def))
           (arity (length (def-params def)))
           (name (and (not (def-anonymous? def)) (string-append proc "_name"))))
      `(,@(if name `((string ,name ,(procedure-name def))) '())
        (rodata ,proc (quad ,procedure-type) (quad ,(if (def-rest? def) (- arity) arity))
                (quad ,(def-code def)) (quad ,(or name 0)) (quad ,(def-captures def)))
        (function ,(def-code def) (arity ,arity)
                  ,@(lower-function (def-code def) blocks #f)))))


  (define (lower-function name blocks noalloc?)
    (let ((head `(function ,name)))
      (dumped 'copy-prop head
              (propagate-copies (dumped 'compile head (map lower-block blocks)) noalloc?)))))
