;; An if in test position branches on its arms
(def pick ((raw a) (raw b)) -> (val) (:inline)
     (if (if (raw= a b) #f (raw< a b)) 1 2))

(def main () -> (val) ()
     (cons (pick (raw 1) (raw 1))
           (cons (pick (raw 1) (raw 2))
                 (if (if #t #f #t) 3 4))))
---
stdout = "(2 1 . 4)"
