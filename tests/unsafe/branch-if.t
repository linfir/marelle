;; An if in test position branches on its arms
(define (pick (raw a) (raw b))
  (declare unsafe inline)
  (if (if (raw= a b) #f (raw< a b)) 1 2))

(define (entry)
  (declare unsafe)
  (cons (pick (raw 1) (raw 1))
        (cons (pick (raw 1) (raw 2))
              (if (if #t #f #t) 3 4))))
(entry)
---
stdout = "(2 1 . 4)"
