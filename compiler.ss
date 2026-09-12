;; compiler.ss

(define (generator prefix)
  (let ((n 0))
    (lambda ()
      (set! n (+ n 1))
      (string-append prefix (number->string n)))))

(define tmp-var (generator "%tmp"))

(define new-label (generator "@label"))

(define (join . xs)
  (apply append
         (map (lambda (x)
                (if (and (pair? x) (symbol? (car x)))
                    (list x)
                    x))
              xs)))

(define (compile-expr dst e)
  (cond
   ((integer? e)
    (list (list 'copy dst e)))

   ((pair? e)
    (case (car e)
      ((+) (compile-binop 'add dst (cadr e) (caddr e)))
      ((-) (compile-binop 'sub dst (cadr e) (caddr e)))
      ((*) (compile-binop 'mul dst (cadr e) (caddr e)))
      ((if) (compile-if dst (cadr e) (caddr e) (cadddr e)))
      (else (error))))

   (else (error))))

(define (compile-binop cmd dst x y)
  (let* ((tx (tmp-var))
         (ty (tmp-var)))
    (join
     (compile-expr tx x)
     (compile-expr ty y)
     (list cmd dst tx ty))))

(define (compile-if dst test-expr true-expr false-expr)
  (let ((true-label0 (new-label))
        (true-label (new-label))
        (false-label0 (new-label))
        (false-label (new-label))
        (join-label (new-label))
        (test-var (tmp-var))
        (true-var (tmp-var))
        (false-var (tmp-var)))
    (join
     (compile-expr test-var test-expr)
     (list 'jnz test-var true-label0 false-label0)

     (list 'label true-label0)
     (compile-expr true-var true-expr)
     (list 'label true-label)
     (list 'jmp join-label)

     (list 'label false-label0)
     (compile-expr false-var false-expr)
     (list 'label false-label)

     (list 'label join-label)
     (list 'phi dst true-label true-var false-label false-var))))


(define (emit xs)
  (for-each emit1 xs))

(define (emit1 e)
  (define (p . x)
    (for-each display x)
    (newline))
  (define (pi . x)
    (display "    ")
    (for-each display x)
    (newline))

  (case (car e)
    ((copy)
     (pi (cadr e) " =l copy " (caddr e)))
    ((add sub mul)
     (pi (cadr e) " =l " (car e) " " (caddr e) ", " (cadddr e)))
    ((label)
     (p (cadr e)))
    ((jmp)
     (pi "jmp " (cadr e)))
    ((jnz)
     (pi "jnz " (cadr e) ", " (caddr e) ", " (cadddr e)))
    ((phi)
     (pi (list-ref e 1) " =l phi " (list-ref e 2) " " (list-ref e 3) ", " (list-ref e 4) " " (list-ref e 5)))
    (else
     (error (list 'emit1 e)))))

(define expr '(if (- 2 1)
                  (* (+ 2 4) (- 8 1))
                  (- 58 100)))

(display "export function w $main() {\n")
(display "@start\n")
(emit (compile-expr "%result" expr))
(display "    call $print_int(l %result)\n")
(display "    ret 0\n")
(display "}\n")
