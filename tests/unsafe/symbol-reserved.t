;; Data named like a symbol of the IR
(define-data scheme-globals 1)

(define (entry)
  (declare unsafe)
  1)
(entry)
---
stage = "compiler"
stderr_contains = "symbol clash"
