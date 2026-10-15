(def swap ((val n) (val a) (val b)) -> (val) ()
     (if (= n 0)
         (cons a b)
         (swap (- n 1) b a)))

(def main () -> (val) ()
     (swap 1000001 1 2))
---
env = { SCHEME_HEAP = "1000", SCHEME_GC_STATS = "1" }
stdout = "(2 . 1)"
stderr_contains = "GC: 0 collections"
gc_stress = "skip"
