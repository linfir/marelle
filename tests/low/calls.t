(def pair-sum ((val p)) -> (val) ()
     (+ (car p) (cdr p)))

(def wrap ((val x)) -> (val) ()
     (cons x (cons x '())))

(def main () -> (val) ()
     (cons (pair-sum (cons 40 2)) (wrap (wrap 7))))
---
stdout = "(42 (7 7) (7 7))"
env = { SCHEME_GC_STRESS = "1" }
