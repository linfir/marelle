;; Noalloc functions take any number of arguments, on the machine stack
(define (f (raw a) (raw b) (raw c) (raw d) (raw e) (raw f) (raw g))
  (declare unsafe noalloc (result raw))
  (raw- g (raw+ a (raw+ b c))))

(define (entry)
  (declare unsafe)
  (make-fixnum (f (raw 1) (raw 2) (raw 3) (raw 4) (raw 5) (raw 6) (raw 70))))
(entry)
---
stdout = "64"
