;; A native keeps values in registers across a call to another native using the same ones
(define (triangle (raw n))
  (declare unsafe native (result raw))
  (let loop ((i n) (s (raw 0)))
    (if (raw= i (raw 0)) s (loop (raw- i (raw 1)) (raw+ s i)))))

(define (around (raw n))
  (declare unsafe native (result raw))
  (let ((p (raw+ n (raw 1)))
        (q (raw+ n (raw 2)))
        (r (raw+ n (raw 3))))
    (raw+ (triangle n) (raw+ p (raw+ q r)))))

(define (entry)
  (declare unsafe)
  (make-fixnum (around (raw 10))))
(entry)
---
stdout = "91"
