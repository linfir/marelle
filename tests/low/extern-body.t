;; Only an inline extern def has a body
(def f ((val x)) -> (val) (:extern)
     x)

(def main () -> (val) ()
     (f 1))
---
stage = "low"
stderr_contains = "body in an extern def"
