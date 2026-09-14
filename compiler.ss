;; compiler.ss


(define (generator prefix)
  (let ((n 0))
    (lambda ()
      (set! n (+ n 1))
      (string-append prefix (number->string n)))))


(define genvar (generator "%tmp"))


(define genlabel (generator "@label"))


(define (seq . xs)
  (cons 'seq
        (apply append
               (map (lambda (x)
                      (if (and (pair? x) (eq? (car x) 'seq))
                          (cdr x)
                          (list x)))
                    xs))))


(define (compile-expr dst env expr)
  (cond
   ((integer? expr)
    (list 'copy dst expr))

   ((and (pair? expr) (symbol? (car expr)))
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
      (else  (apply compile-call  dst env expr))))

   ((symbol? expr)
    (let ((var (env-lookup env expr)))
      (if var
          (list 'copy dst var)
          (error (list 'undefined expr)))))

   (else (error (list 'compile-expr expr)))))


(define (compile-define name params body)
  (let ((env (map (lambda (var) (cons var (genvar))) params)))
    (list 'function name (map cdr env)
          (seq (list 'label "@start")
               (compile-expr "%result" env body)
               (list 'return "%result")))))


(define (compile-call dst env name . args)
  (define (f x)
    (let ((tmp (genvar)))
      (cons tmp (compile-expr tmp env x))))

  (let ((alist (map f args)))
    (let ((vars (map car alist))
          (bodies (map cdr alist)))
      (seq (apply seq bodies)
           (list 'call dst name vars)))))



(define (compile-binop dst env cmd x y)
  (let* ((tx (genvar))
         (ty (genvar)))
    (seq (compile-expr tx env x)
         (compile-expr ty env y)
         (list cmd dst tx ty))))


(define (compile-if dst env test-expr true-expr false-expr)
  (let ((true-label0  (genlabel))
        (true-label   (genlabel))
        (false-label0 (genlabel))
        (false-label  (genlabel))
        (join-label   (genlabel))
        (test-var  (genvar))
        (true-var  (genvar))
        (false-var (genvar)))
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


(define (env-lookup env var)
  (let ((p (assq var env)))
    (if p
        (cdr p)
        #f)))


(define (compile-let dst env bindings expr)
  (let loop ((bindings bindings)
             (code '())
             (new-env env))
    (if (null? bindings)
        (seq (apply seq (reverse code))
             (compile-expr dst new-env expr))
        (let ((var (genvar)))
          (loop (cdr bindings)
                (cons (compile-expr var env (cadar bindings)) code)
                (extend-env (caar bindings) var new-env))))))


(define (display-args xs)
  (display "(")
  (let loop ((first #t) (xs xs))
    (unless (null? xs)
      (unless first
        (display ", "))
      (display "l ")
      (display (car xs))
      (loop #f (cdr xs))))
  (display ")")
  )


(define (emit expr)
  (define (p . x)
    (for-each display x)
    (newline))

  (define (pi . x)
    (display "    ")
    (apply p x))

  (unless (null? expr)
    (case (car expr)
      ((function)
       (let ((name (list-ref expr 1))
             (vars (list-ref expr 2))
             (body (list-ref expr 3)))
         (if (eq? name 'main)
             (display "export function w $main () {\n")
             (begin
               (display "function l $")
               (display name)
               (display " ")
               (display-args vars)
               (display " {\n")))
         (emit body)
         (display "}\n\n")))

      ((call)
       (display "    ")
       (display (cadr expr))
       (display " =l call $")
       (display (caddr expr))
       (display-args (cadddr expr))
       (newline))

      ((seq)
       (for-each emit (cdr expr)))
      ((return)
       (pi "ret " (cadr expr)))
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
  (define (main expr)
    (display "export function w $main() {\n")
    (display "@start\n")
    (emit (compile-expr "%result" '() expr))
    (display "    call $print_int(l %result)\n")
    (display "    ret 0\n")
    (display "}\n"))

  (map (lambda (x)
         (if (and (pair? x)
                  (eq? (car x) 'define))
             (emit (compile-define (caadr x) (cdadr x) (caddr x)))
             (main x)))
       expr))


(define (read-all)
  (let loop ((acc '()))
    (let ((x (read)))
      (if (eof-object? x)
          (reverse acc)
          (loop (cons x acc))))))


(compile-program (read-all))
