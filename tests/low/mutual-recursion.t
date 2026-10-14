(def even? ((val n)) -> (val) ()
     (if (= n 0) #t (odd? (- n 1))))

(def odd? ((val n)) -> (val) ()
     (if (= n 0) #f (even? (- n 1))))

(def main () -> (val) ()
     (cons (even? 10) (odd? 7)))
---
stdout = "(#t . #t)"
