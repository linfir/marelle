(def f ((val x)) -> (val) ((:c-name "f"))
     x)

(def main () -> (val) ()
     (f 1))
---
stage = "low"
stderr_contains = ":c-name requires :native"
