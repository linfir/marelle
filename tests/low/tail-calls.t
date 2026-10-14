(def even? ((val n)) -> (val) ()
     (if (= n 0) #t (odd? (- n 1))))

(def odd? ((val n)) -> (val) ()
     (if (= n 0) #f (even? (- n 1))))

(def step ((val n)) -> (val) (:inline)
     (count (- n 1)))

;; Tail position through let, let*, begin and an inline def
(def count ((val n)) -> (val) ()
     (if (= n 0)
         0
         (let ((m n))
           (let* ((k m))
             (begin
               (car (cons 1 2))
               (step k))))))

(def main () -> (val) ()
     (count 1000000)
     (cons (even? 1000000) (odd? 1000001)))
---
env = { SCHEME_HEAP = "5000" }
stdout = "(#t . #t)"
