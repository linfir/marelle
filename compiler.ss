;; compiler.ss

(define (emit x)
  (display x)
  (newline))

(define (emit-i x)
  (display "    ")
  (emit x))


(emit "export function w $main() {")
(emit "@start")
(emit-i "%x =l add 40, 2")
(emit-i "call $print_int(l %x)")
(emit-i "ret 0")
(emit "}")
