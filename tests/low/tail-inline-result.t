;; An inline call ending a def is checked against the def's result
(def f ((val x)) -> (raw) (:native)
     (car x))

(def main () -> (val) ()
     (as-val (f (cons 1 2))))
---
stage = "low"
stderr_contains = "expected a raw"
