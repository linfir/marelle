(define (call f . args) (apply f args))
(list (call member 2 (list 1 2)) (call member 2 (list 1 3) <) (map (lambda (x) (assoc x (list (cons 1 'a)))) (list 1 2)))
