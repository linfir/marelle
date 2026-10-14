;; A non-public def named for Scheme, and an anonymous one
(def f.1 ((val x)) -> (val) ((:as f))
     x)

(def lambda.2 () -> (val) (:anonymous)
     1)

(def main () -> (val) ()
     (cons f.1 (cons lambda.2 (cons (lambda.2) '()))))
---
stdout = "(#<procedure f> #<procedure> 1)"
