;; Lifted body of (lambda (x) (+ x k)) capturing k
(def adder ((val x)) -> (val) ((:captures 1))
     (+ x (closure-ref (raw 0))))

;; Lifted body of (lambda () (cons a b)) capturing a and b
(def pairer () -> (val) (:anonymous (:captures 2))
     (cons (closure-ref (raw 0)) (closure-ref (raw 1))))

(def make-adder ((val k)) -> (val) ()
     (let ((c (alloc-closure adder)))
       (store c (raw 5) k)
       (as-val c)))

(def make-pairer ((val a) (val b)) -> (val) ()
     (let ((c (alloc-closure pairer)))
       (store c (raw 5) a)
       (store c (raw 6) b)
       (as-val c)))

;; Allocates closures to force collections while some are live
(def sum ((val n) (val acc)) -> (val) ()
     (if (= n 0)
         acc
         (sum (- n 1) ((make-adder n) acc))))

(def main () -> (val) ()
     (let ((f (make-adder 10))
           (p (make-pairer 1 (cons 2 '()))))
       (sum 100000 0)
       (cons (f 5) (cons (p) (cons f (cons p '()))))))
---
env = { SCHEME_HEAP = "5000" }
stdout = "(15 (1 2) #<procedure adder> #<procedure>)"
