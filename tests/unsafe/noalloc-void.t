(define (show (raw n))
  (declare unsafe noalloc (result void))
  (put-int stdout n)
  (put-string stdout " "))

(define (entry)
  (declare unsafe)
  (show (raw 7))
  0)
(entry)
---
stdout = "7 0"
