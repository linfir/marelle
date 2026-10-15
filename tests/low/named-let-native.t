;; A raw loop in a native def
(def triangle ((raw n)) -> (raw) (:native)
     (let loop ((i n) (acc (raw 0)))
       (if (raw= i (raw 0))
           acc
           (loop (raw- i (raw 1)) (raw+ acc i)))))

(def main () -> (val) ()
     (as-val (raw+ (raw<< (triangle (raw 100)) (raw 3)) (raw 1))))
---
stdout = "5050"
