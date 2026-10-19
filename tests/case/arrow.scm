;; R7RS => clauses (Chez follows R6RS, which has none)
(cons (case 5 ((5) => (lambda (x) (* x 10))) (else 0))
      (case 6 ((5) 1) (else => (lambda (x) (+ x 1)))))
