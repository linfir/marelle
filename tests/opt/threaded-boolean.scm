;; A predicate's boolean, bound then tested, branches straight from the type test
(define (f x) (let ((b (pair? x))) (if b 1 2)))
(list (f '(1)) (f 5))
