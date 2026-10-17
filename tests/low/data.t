;; Data words start at zero and keep their values across calls
(data counter 1 ())
(data table 3 ())

(def bump ((raw i)) -> (raw) (:native)
     (store table i (raw+ (load table i) (raw 1)))
     (store counter (raw 0) (raw+ (load counter (raw 0)) (raw 1)))
     (load table i))

(def main () -> (val) ()
     (bump (raw 2))
     (bump (raw 0))
     (let ((a (bump (raw 2))))
       (as-val (raw+ (raw<< (raw+ (raw* a (raw 10)) (load counter (raw 0))) (raw 3))
                     (raw 1)))))
---
stdout = "23"
