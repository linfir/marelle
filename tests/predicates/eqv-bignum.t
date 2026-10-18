(list (eqv? (expt 2 100) (expt 2 100)) (eq? (expt 2 100) (expt 2 100)) (eqv? (expt 2 100) (- (expt 2 100))) (eqv? (expt 2 100) (expt 2 101)) (eqv? (expt 2 100) 1) (eqv? 1 (expt 2 100)))
---
stdout = "(#t #f #f #f #f #f)"
