;; Data named like a symbol of the IR
(data scheme-globals 1 ())

(def main () -> (val) ()
     1)
---
stage = "low"
stderr_contains = "symbol clash"
