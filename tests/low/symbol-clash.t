;; a-b and a_b both give the symbol a_b_code
(def a-b ((val x)) -> (val) ()
     x)

(def a_b ((val x)) -> (val) ()
     x)

(def main () -> (val) ()
     (a-b (a_b 1)))
---
stage = "low"
stderr_contains = "symbol clash"
