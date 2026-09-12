;; compiler.ss


(define tmp 0)

(define (tmp-var)
  (let ((n tmp))
    (set! tmp (+ tmp 1))
    (string-append "%tmp" (number->string n))))

(define (compile-expr dst e)
  (cond
   ((integer? e)
    (list (list 'copy dst e)))

   ((pair? e)
    (case (car e)
      ((+) (compile-binop 'add dst (cadr e) (caddr e)))
      ((-) (compile-binop 'sub dst (cadr e) (caddr e)))
      ((*) (compile-binop 'mul dst (cadr e) (caddr e)))
      (else (error))))

   (else (error))))

(define (compile-binop cmd dst x y)
  (let* ((tx (tmp-var))
         (ty (tmp-var)))
    (append
     (compile-expr tx x)
     (compile-expr ty y)
     (list (list cmd dst tx ty)))))

(define (emit xs)
  (for-each emit1 xs))

(define (emit1 e)
  (display "    # ")
  (display e)
  (newline)
  (case (car e)
    ((copy)
     (display "    ")
     (display (cadr e))
     (display " =l copy ")
     (display (caddr e))
     (newline))
    ((add sub mul)
     (display "    ")
     (display (cadr e))
     (display " =l ")
     (display (car e))
     (display " ")
     (display (caddr e))
     (display ", ")
     (display (cadddr e))
     (newline))))

(define expr '(* (+ 2 4) (- 8 1)))

(display "export function w $main() {\n")
(display "@start\n")
(emit (compile-expr "%result" expr))
(display "    call $print_int(l %result)\n")
(display "    ret 0\n")
(display "}\n")
