;; A closure def is only a template, not a procedure value
(def adder ((val x)) -> (val) ((:captures 1))
     (+ x (closure-ref (raw 0))))

(def main () -> (val) ()
     (cons adder '()))
---
stage = "low"
stderr_contains = "expected a val"
