(call-with-values
    (lambda () (partition even? '(1 2 3 4 5 6)))
  (lambda (in out) (list in out)))
