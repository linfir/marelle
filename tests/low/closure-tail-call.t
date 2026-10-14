(def adder ((val x)) -> (val) ((:captures 1))
     (+ x (closure-ref (raw 0))))

(def call ((val x)) -> (val) ()
     (adder x))

(def main () -> (val) ()
     (call 1))
---
stage = "low"
stderr_contains = "direct call of a closure def"
