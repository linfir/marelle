;; Loop variables survive collections
(define (entry)
  (declare unsafe)
  (let loop ((i 0) (l '()))
    (if (= i 1000)
        (cons (car l) (car (cdr l)))
        (loop (+ i 1) (cons i l)))))
(entry)
---
stdout = "(999 . 998)"
env = { SCHEME_GC_STRESS = "1" }
