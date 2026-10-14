(def add1 ((val x)) -> (val) ()
     (+ x 1))

(def twice ((val f) (val x)) -> (val) ()
     (f (f x)))

;; Indirect tail call in a loop
(def repeat ((val f) (val n) (val x)) -> (val) ()
     (if (= n 0)
         x
         (repeat f (- n 1) (f x))))

(def count ((val n)) -> (val) ()
     (if (= n 0) 0 ((car (cons count '())) (- n 1))))

(def main () -> (val) ()
     (let ((g add1))
       (count 1000000)
       (cons (g 1)
             (cons (twice add1 5)
                   (cons (repeat add1 1000 0) twice)))))
---
env = { SCHEME_HEAP = "5000" }
stdout = "(2 7 1000 . #<procedure twice>)"
