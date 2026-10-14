;; Allocates boxes to force collections while one is live
(def churn ((val n)) -> (val) ()
     (if (= n 0)
         0
         (begin (make-box n) (churn (- n 1)))))

(def main () -> (val) ()
     (let ((b (make-box (cons 1 2))))
       (churn 100000)
       (box-set! b (cons (box-ref b) 3))
       (churn 100000)
       (box-ref b)))
---
env = { SCHEME_HEAP = "5000" }
stdout = "((1 . 2) . 3)"
