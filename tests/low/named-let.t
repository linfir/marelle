;; A loop in tail position
(def main () -> (val) ()
     (let loop ((i 0) (acc 0))
       (if (= i 10)
           acc
           (loop (+ i 1) (+ acc i)))))
---
stdout = "45"
