(def fact ((val n)) -> (val) ()
     (if (= n 0)
         1
         (* n (fact (- n 1)))))

(def main () -> (val) ()
     (fact 10))
---
stdout = "3628800"
