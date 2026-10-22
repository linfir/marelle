;; The hash procedure inserts into its own table the first time, resizing it while the outer
;; hashtable-set! is between its growth and its probe
(define t #f)
(define inside? #f)
(define (hash k)
  (unless inside?
    (set! inside? #t)
    (let fill ((i 100))
      (when (< i 140)
        (hashtable-set! t i i)
        (fill (+ i 1)))))
  (if (fixnum? k) k 0))
(set! t (make-hashtable hash =))
(hashtable-set! t 1 'one)
(list (hashtable-size t) (hashtable-ref t 1 #f) (hashtable-ref t 139 #f)
      (length (vector->list (hashtable-keys t))))
