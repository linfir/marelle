;; A closure called before a later lambda capturing it is created: the later one's captures
;; are not yet bound at that call, so neither is lifted with the other
(define (f k)
  (define (add x) (+ x k))
  (let ((a (add 1)))
    (let ((m (* a 2)))
      (let ((g (lambda (y) (add (+ y m)))))
        (list a (g 3) (map g '(1 2)))))))
(f 10)
