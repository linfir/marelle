;; compiler.ss

(define tmp 0)

(define (fresh-var)
  (let ((n tmp))
    (set! tmp (+ tmp 1))
    (string-append "%t" (number->string n))))

(define (compile-expr e dst)
  (cond
   ((integer? e)
    (emit-i (string-append dst " =l copy " (number->string e))))
   ((pair? e)
    (cond
     ((eq? (car e) '+)
      (let* ((x (fresh-var))
             (y (fresh-var)))
        (compile-expr (cadr e) x)
        (compile-expr (caddr e) y)
        (emit-i (string-append dst " =l add " x ", " y))))
     (else (error 'cannot-compile))))
   (else (error 'cannot-compile))))

(define (emit x)
  (display x)
  (newline))

(define (emit-i x)
  (display "    ")
  (emit x))


(emit "export function w $main() {")
(emit "@start")
(compile-expr '(+ (+ 30 10) 2) "%x")
(emit-i "call $print_int(l %x)")
(emit-i "ret 0")
(emit "}")
