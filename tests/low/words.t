;; fill-words then copy-words of the middle three words
(data word-source 5 ())
(data word-target 5 ())

(def word-sum ((raw p) (raw n)) -> (raw) (:native)
     (let loop ((i (raw 0))
                (sum (raw 0)))
       (if (raw= i n)
           sum
           (loop (raw+ i (raw 1)) (raw+ sum (load p i))))))

(def main () -> (val) ()
     (fill-words word-source (raw 5) (raw 7))
     (store word-source (raw 2) (raw 100))
     (copy-words (raw+ word-target (raw 8)) (raw+ word-source (raw 8)) (raw 3))
     (as-val (raw+ (raw<< (word-sum word-target (raw 5)) (raw 3)) (raw 1))))
---
stdout = "114"
