;; The inits are evaluated left to right
(def show ((val x)) -> (val) ()
     (display (stdout) x)
     x)

(def main () -> (val) ()
     (let loop ((a (show 1)) (b (show 2)) (c (show 3)) (d (show 4)) (e (show 5)))
       (put-string (stdout) " ")
       a))
---
stdout = "12345 1"
