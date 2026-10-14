(def main () -> (val) ()
     (let ((x 1))
       (let* ((y 2)
              (x 3))
         (set! x 4)
         (set! y x))
       x))
---
stdout = "1"
