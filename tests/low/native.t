;; Raw arguments and results, recursion, and a raw live across a native call
(def fact ((raw n)) -> (raw) (:native)
     (if (raw= n (raw 0))
         (raw 1)
         (raw* n (fact (raw- n (raw 1))))))

(def main () -> (val) ()
     (let ((a (raw 2)))
       (let ((b (fact (raw 5))))
         (as-val (raw+ (raw<< (raw+ a b) (raw 3)) (raw 1))))))
---
stdout = "122"
