(def adder ((val x)) -> (val) ((:captures 1))
     (+ x (closure-ref (raw 0))))

(def main () -> (val) ()
     (adder 1))
---
stage = "low"
stderr_contains = "direct call of a closure def"
