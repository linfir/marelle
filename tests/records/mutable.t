(define-record-type interval (fields var (mutable start) (mutable end)))
(define i (make-interval 'a 1 2))
(interval-start-set! i 10)
(interval-end-set! i (+ (interval-end i) 1))
(list (interval-var i) (interval-start i) (interval-end i))
---
stdout = "(a 10 3)"
