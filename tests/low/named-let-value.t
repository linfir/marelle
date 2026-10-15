;; A loop as a value, exiting through if, let and begin
(def main () -> (val) ()
     (cons (let loop ((i 0))
             (if (= i 3)
                 (let ((j (+ i 1)))
                   (begin j))
                 (loop (+ i 1))))
           7))
---
stdout = "(4 . 7)"
