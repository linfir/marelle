;; The inlined body uses the caller's names for its own variables
(def f ((val x) (val y)) -> (val) (:inline)
     (let ((z (+ x 1)))
       (set! x 100)
       (cons x (cons y (cons z '())))))

(def main () -> (val) ()
     (let ((x 1) (z 2) (y 3))
       (cons (f z x) (cons x (cons y (cons z '()))))))
---
stdout = "((100 1 3) 1 3 2)"
