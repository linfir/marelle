;; compiler.ss


(define (generator prefix)
  (let ((n 0))
    (lambda ()
      (set! n (+ n 1))
      (string-append prefix (number->string n)))))


(define genvar   (generator "%tmp"))
(define genlabel (generator "@label"))
(define genfn    (generator "$f"))


(define (seq . xs)
  (cons 'seq
        (apply append
               (map (lambda (x)
                      (if (and (pair? x) (eq? (car x) 'seq))
                          (cdr x)
                          (list x)))
                    xs))))


(define (compile-expr dst env fenv expr)
  (cond
   ((integer? expr)
    (list 'copy dst expr))

   ((and (pair? expr) (symbol? (car expr)))
    (case (car expr)
      ((+)   (apply compile-binop dst env fenv 'add   (cdr expr)))
      ((-)   (apply compile-binop dst env fenv 'sub   (cdr expr)))
      ((*)   (apply compile-binop dst env fenv 'mul   (cdr expr)))
      ((=)   (apply compile-binop dst env fenv 'ceql  (cdr expr)))
      ((<)   (apply compile-binop dst env fenv 'csltl (cdr expr)))
      ((>)   (apply compile-binop dst env fenv 'csgtl (cdr expr)))
      ((<=)  (apply compile-binop dst env fenv 'cslel (cdr expr)))
      ((>=)  (apply compile-binop dst env fenv 'csgel (cdr expr)))
      ((if)  (apply compile-if    dst env fenv (cdr expr)))
      ((let) (apply compile-let   dst env fenv (cdr expr)))
      (else  (apply compile-call  dst env fenv expr))))

   ((symbol? expr)
    (let ((var (env-lookup env expr)))
      (if var
          (list 'copy dst var)
          (error (list 'undefined expr)))))

   (else (error (list 'compile-expr expr)))))


(define (compile-function name params fenv body)
  (let ((env (map (lambda (var)
                    (cons var (genvar)))
                  params)))
    (list 'function (list-ref (assq name fenv) 1) (map cdr env)
          (seq (list 'label "@start")
               (compile-expr "%result" env fenv body)
               (list 'return "%result")))))


(define (compile-call dst env fenv name . args)
  (let ((info (assoc name fenv)))
    (unless info
      (error (list 'undefined-function name)))
    (unless (= (length args) (list-ref info 2))
      (error (list 'function-call-invalid-arity)))

    (let ((alist (map (lambda (x)
                        (let ((tmp (genvar)))
                          (cons tmp (compile-expr tmp env fenv x))))
                      args)))
      (let ((vars (map car alist))
            (bodies (map cdr alist)))
        (seq (apply seq bodies)
             (list 'call dst (list-ref info 1) vars))))))


(define (compile-binop dst env fenv cmd x y)
  (let* ((tx (genvar))
         (ty (genvar)))
    (seq (compile-expr tx env fenv x)
         (compile-expr ty env fenv y)
         (list cmd dst tx ty))))


(define (compile-if dst env fenv test-expr true-expr false-expr)
  (let ((true-label0  (genlabel))
        (true-label   (genlabel))
        (false-label0 (genlabel))
        (false-label  (genlabel))
        (join-label   (genlabel))
        (test-var  (genvar))
        (true-var  (genvar))
        (false-var (genvar)))
    (seq
     (compile-expr test-var env fenv test-expr)
     (list 'jnz test-var true-label0 false-label0)

     (list 'label true-label0)
     (compile-expr true-var env fenv true-expr)
     (list 'label true-label)
     (list 'jmp join-label)

     (list 'label false-label0)
     (compile-expr false-var env fenv false-expr)
     (list 'label false-label)

     (list 'label join-label)
     (list 'phi dst true-label true-var false-label false-var))))


(define (extend-env env0 var val)
  (cons (cons var val) env0))


(define (env-lookup env var)
  (let ((p (assq var env)))
    (if p
        (cdr p)
        #f)))


(define (compile-let dst env fenv bindings expr)
  (let loop ((bindings bindings)
             (code '())
             (new-env env))
    (if (null? bindings)
        (seq (apply seq (reverse code))
             (compile-expr dst new-env fenv expr))
        (let ((var (genvar)))
          (loop (cdr bindings)
                (cons (compile-expr var env fenv (cadar bindings)) code)
                (extend-env new-env (caar bindings) var))))))


;; x must be a non-empty list
(define (split-last x)
  (let loop ((x x) (acc '()))
    (if (null? (cdr x))
        (cons (reverse acc) (car x))
        (loop (cdr x) (cons (car x) acc)))))


(define (display-args xs)
  (display "(")
  (let loop ((first #t) (xs xs))
    (unless (null? xs)
      (unless first
        (display ", "))
      (display "l ")
      (display (car xs))
      (loop #f (cdr xs))))
  (display ")"))


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
               (display "function l ")
               (display name)
               (display " ")
               (display-args vars)
               (display " {\n")))
         (emit body)
         (display "}\n\n")))

      ((call)
       (display "    ")
       (display (cadr expr))
       (display " =l call ")
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


(define (compile-program program)
  (define (emit-main fenv expr)
    (display "export function w $main() {\n")
    (display "@start\n")
    (emit (compile-expr "%result" '() fenv expr))
    (display "    call $print_int(l %result)\n")
    (display "    ret 0\n")
    (display "}\n"))

  (define (emit-def fenv expr)
    (emit (compile-function (caadr expr) (cdadr expr) fenv (caddr expr))))

  (define (scan defs)
    (let loop ((defs defs)
               (fenv '()))
      (if (null? defs)
          fenv
          (let ((def (car defs)))
            (unless (and (pair? def)
                         (eq? (car def) 'define))
              (error 'not-a-def))
            (let ((fn (genfn))
                  (name (caadr def))
                  (args (cdadr def)))
              (when (assq name fenv)
                (error 'redefinition-not-allowed))
              (loop (cdr defs)
                    (cons (list name fn (length args)) fenv)))))))

  (let* ((program2 (split-last program))
         (defs (car program2))
         (expr (cdr program2))
         (fenv (scan defs)))
    (for-each (lambda (x) (emit-def fenv x)) defs)
    (emit-main fenv expr)))


(define (read-all)
  (let loop ((acc '()))
    (let ((x (read)))
      (if (eof-object? x)
          (reverse acc)
          (loop (cons x acc))))))


(compile-program (read-all))
