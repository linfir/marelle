;; compiler.ss


(define (generator prefix)
  (let ((n 0))
    (lambda ()
      (set! n (+ n 1))
      (string-append prefix (number->string n)))))


(define tmp-var (generator "%tmp"))


(define new-label (generator "@label"))


(define (seq . xs)
  (cons 'seq xs))


(define (compile-expr dst env expr)
  (cond
   ((integer? expr)
    (list 'copy dst expr))

   ((pair? expr)
    (case (car expr)
      ((+)   (apply compile-binop dst env 'add   (cdr expr)))
      ((-)   (apply compile-binop dst env 'sub   (cdr expr)))
      ((*)   (apply compile-binop dst env 'mul   (cdr expr)))
      ((=)   (apply compile-binop dst env 'ceql  (cdr expr)))
      ((<)   (apply compile-binop dst env 'csltl (cdr expr)))
      ((>)   (apply compile-binop dst env 'csgtl (cdr expr)))
      ((<=)  (apply compile-binop dst env 'cslel (cdr expr)))
      ((>=)  (apply compile-binop dst env 'csgel (cdr expr)))
      ((if)  (apply compile-if    dst env (cdr expr)))
      ((let) (apply compile-let   dst env (cdr expr)))
      (else (error))))

   ((symbol? expr)
    (let ((p (assq expr env)))
      (if p
          (list 'copy dst (cdr p))
          (error 'undefined expr))))

   (else (error))))


(define (compile-binop dst env cmd x y)
  (let* ((tx (tmp-var))
         (ty (tmp-var)))
    (seq (compile-expr tx env x)
         (compile-expr ty env y)
         (list cmd dst tx ty))))


(define (compile-if dst env test-expr true-expr false-expr)
  (let ((true-label0  (new-label))
        (true-label   (new-label))
        (false-label0 (new-label))
        (false-label  (new-label))
        (join-label   (new-label))
        (test-var  (tmp-var))
        (true-var  (tmp-var))
        (false-var (tmp-var)))
    (seq
     (compile-expr test-var env test-expr)
     (list 'jnz test-var true-label0 false-label0)

     (list 'label true-label0)
     (compile-expr true-var env true-expr)
     (list 'label true-label)
     (list 'jmp join-label)

     (list 'label false-label0)
     (compile-expr false-var env false-expr)
     (list 'label false-label)

     (list 'label join-label)
     (list 'phi dst true-label true-var false-label false-var))))




(define (extend-env var val env0)
  (cons (cons var val) env0))


(define (compile-let dst env bindings expr)
  (let loop ((bindings bindings)
             (code '())
             (new-env env))
    (if (null? bindings)
        (seq (apply seq (reverse code))
             (compile-expr dst new-env expr))
        (let ((var (tmp-var)))
          (loop (cdr bindings)
                (cons (compile-expr var env (cadar bindings)) code)
                (extend-env (caar bindings) var new-env))))))


(define (emit expr)
  (define (p . x)
    (for-each display x)
    (newline))
  (define (pi . x)
    (display "    ")
    (apply p x))

  (unless (null? expr)
    (case (car expr)
      ((seq)
       (for-each emit (cdr expr)))
      ((copy)
       (pi (cadr expr) " =l copy " (caddr expr)))
      ((add sub mul ceql csltl csgtl cslel csgel)
       (pi (cadr expr) " =l " (car expr) " " (caddr expr) ", " (cadddr expr)))
      ((label)
       (p (cadr expr)))
      ((jmp)
       (pi "jmp " (cadr expr)))
      ((jnz)
       (pi "jnz " (cadr expr) ", " (caddr expr) ", " (cadddr expr)))
      ((phi)
       (pi (list-ref expr 1) " =l phi "
           (list-ref expr 2) " " (list-ref expr 3) ", "
           (list-ref expr 4) " " (list-ref expr 5)))
      (else
       (error (list 'emit expr))))))


(define (compile-program expr)
  (display "export function w $main() {\n")
  (display "@start\n")
  (emit (compile-expr "%result" '() expr))
  (display "    call $print_int(l %result)\n")
  (display "    ret 0\n")
  (display "}\n"))


(compile-program (read))
