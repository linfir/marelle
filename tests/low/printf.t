;; A variadic C call
(def main () -> (val) ()
     (c-call printf "%ld-%s " (raw 42) "ok")
     0)
---
stdout = "42-ok 0"
