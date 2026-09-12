;; compiler.ss

(define tmp 0)

(define (tmp-var)
  (let ((n tmp))
    (set! tmp (+ tmp 1))
    (string-append "%t" (number->string n))))

(define (compile-expr e dst)
  (cond
   ((integer? e)
    (list (list 'copy dst e)))
   ((pair? e)
    (case (car e)
     ((+)
      (let* ((x (tmp-var))
             (y (tmp-var)))
        (append
         (compile-expr (cadr e) x)
         (compile-expr (caddr e) y)
         (list (list 'add dst x y)))))
     (else (error 'cannot-compile))))
   (else (error 'cannot-compile))))



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
    ((add)
     (display "    ")
     (display (cadr e))
     (display " =l add ")
     (display (caddr e))
     (display ", ")
     (display (cadddr e))
     (newline)))

  )


(display "export function w $main() {\n")
(display "@start\n")
(emit (compile-expr '(+ (+ 30 10) 2) "%x"))
(display "    call $print_int(l %x)\n")
(display "    ret 0\n")
(display "}\n")
